"""
Modern Retry Decorator for BZA API Classes

This module provides annotation-based retry logic that:
- Retries on IOError, URLError, SSLError, ReadTimeout
- Retries on HTTP 5xx errors (server errors)
- Retries on HTTP 429 (rate limiting)
- Does NOT retry on 4xx errors (client errors)
- Supports exponential backoff with optional jitter
- Keeps transient methods (send_kpi_data, upload_file) silent on final failure
- Logs all retry attempts
"""

import functools
import logging
import time
from typing import Callable, Optional, Tuple, Type

from requests.exceptions import ReadTimeout, RequestException
from urllib.error import URLError
from ssl import SSLError
from urllib3.exceptions import ProtocolError, ReadTimeoutError

from bzt import TaurusNetworkError

LOG = logging.getLogger("BZA.Retry")

# Network problems that should trigger retry
NETWORK_PROBLEMS = (IOError, URLError, SSLError, ReadTimeout, TaurusNetworkError, ProtocolError, ReadTimeoutError)

# HTTP status codes that should trigger retry
RETRIABLE_STATUS_CODES = (429, 500, 501, 502, 503, 504)

# HTTP status codes that should NOT retry
NON_RETRIABLE_STATUS_CODES = (400, 401, 403, 404, 405, 406, 409, 410)


class RetryConfig:
    """Configuration for retry behavior"""

    def __init__(
        self,
        max_attempts: int = 3,
        initial_delay: float = 1.0,
        backoff_factor: float = 2.0,
        max_delay: float = 60.0,
        jitter: bool = False,
        transient: bool = False,
        retriable_exceptions: Tuple[Type[Exception], ...] = NETWORK_PROBLEMS,
    ):
        self.max_attempts = max_attempts
        self.initial_delay = initial_delay
        self.backoff_factor = backoff_factor
        self.max_delay = max_delay
        self.jitter = jitter
        self.transient = transient
        self.retriable_exceptions = retriable_exceptions

    def calculate_delay(self, attempt: int) -> float:
        """Calculate delay for given attempt number (0-indexed)"""
        delay = self.initial_delay * (self.backoff_factor ** attempt)
        delay = min(delay, self.max_delay)

        if self.jitter:
            import random
            jitter_factor = random.uniform(0.75, 1.25)
            delay *= jitter_factor

        return delay


def is_retriable_exception(exc: Exception) -> bool:
    """Check if exception should trigger retry"""
    return isinstance(exc, NETWORK_PROBLEMS)


def is_retriable_status(status_code: int) -> bool:
    """Check if HTTP status code should trigger retry"""
    return status_code in RETRIABLE_STATUS_CODES or (500 <= status_code < 600)


def extract_status_from_error(error: Exception) -> Optional[int]:
    """Extract HTTP status code from exception if present"""
    if hasattr(error, 'status_code'):
        return error.status_code

    if hasattr(error, 'response') and hasattr(error.response, 'status_code'):
        return error.response.status_code

    return None


def should_retry(exc: Exception, config: RetryConfig) -> bool:
    """
    Determine if exception should trigger retry

    Args:
        exc: The exception that occurred
        config: Retry configuration

    Returns:
        True if should retry, False if should fail
    """
    # Check if it's a retriable exception type
    if is_retriable_exception(exc):
        return True

    # Check for HTTP status codes in the error
    status_code = extract_status_from_error(exc)
    if status_code is not None:
        if status_code in NON_RETRIABLE_STATUS_CODES:
            return False
        if is_retriable_status(status_code):
            return True

    # For RequestException, check response status
    if isinstance(exc, RequestException):
        if hasattr(exc, 'response') and exc.response is not None:
            if is_retriable_status(exc.response.status_code):
                return True
            if exc.response.status_code in NON_RETRIABLE_STATUS_CODES:
                return False

    return False


def retry(
    max_attempts: int = 3,
    initial_delay: float = 1.0,
    backoff_factor: float = 2.0,
    max_delay: float = 60.0,
    jitter: bool = False,
    transient: bool = False,
):
    """
    Decorator for retrying methods with exponential backoff

    Args:
        max_attempts: Maximum number of attempts (default 3)
        initial_delay: Initial delay between retries in seconds (default 1.0)
        backoff_factor: Exponential backoff multiplier (default 2.0)
        max_delay: Maximum delay between retries (default 60.0)
        jitter: Add random jitter to delays (default False)
        transient: If True, silently fail on final attempt (default False)
    """
    config = RetryConfig(
        max_attempts=max_attempts,
        initial_delay=initial_delay,
        backoff_factor=backoff_factor,
        max_delay=max_delay,
        jitter=jitter,
        transient=transient,
    )

    def decorator(func: Callable) -> Callable:
        @functools.wraps(func)
        def wrapper(self, *args, **kwargs):
            logger = logging.getLogger(f"{self.__class__.__name__}.{func.__name__}")
            last_exception = None

            for attempt in range(config.max_attempts):
                try:
                    if attempt > 0:
                        logger.info(
                            f"Retry attempt {attempt + 1}/{config.max_attempts} for {func.__name__}"
                        )

                    return func(self, *args, **kwargs)

                except (Exception, BaseException) as exc:
                    last_exception = exc

                    # Check if this exception should trigger a retry
                    if not should_retry(exc, config):
                        logger.debug(
                            f"Error in {func.__name__} is not retriable: {exc.__class__.__name__}"
                        )
                        raise

                    # Check if we have more attempts
                    if attempt >= config.max_attempts - 1:
                        logger.warning(
                            f"All {config.max_attempts} attempts failed for {func.__name__}: {exc}"
                        )

                        if config.transient:
                            logger.info(
                                f"Transient operation {func.__name__} failed after {config.max_attempts} attempts, "
                                f"but continuing (transient=True)"
                            )
                            return None
                        else:
                            raise

                    # Calculate delay for next attempt
                    delay = config.calculate_delay(attempt)
                    logger.debug(
                        f"Attempt {attempt + 1} failed with {exc.__class__.__name__}, "
                        f"retrying in {delay:.2f}s: {exc}"
                    )

                    # Sleep before retry
                    time.sleep(delay)

            # Should never reach here, but just in case
            if last_exception:
                raise last_exception

        wrapper._retry_config = config
        return wrapper

    return decorator


# Convenience decorators for common patterns

def retry_once(transient: bool = False):
    """Retry once (2 total attempts) with 1 second delay"""
    return retry(max_attempts=2, initial_delay=1.0, backoff_factor=1.0, transient=transient)


def retry_aggressive(transient: bool = False):
    """Aggressive retry: 5 attempts with exponential backoff and jitter"""
    return retry(
        max_attempts=5,
        initial_delay=0.5,
        backoff_factor=1.5,
        max_delay=60.0,
        jitter=True,
        transient=transient
    )


def retry_gentle(transient: bool = False):
    """Gentle retry: 3 attempts with slow backoff, good for rate limiting"""
    return retry(
        max_attempts=3,
        initial_delay=2.0,
        backoff_factor=2.0,
        max_delay=60.0,
        jitter=True,
        transient=transient
    )


def retry_transient(max_attempts: int = 3):
    """Retry with silent failure (for data submission operations)"""
    return retry(max_attempts=max_attempts, backoff_factor=2.0, transient=True)


def retry_critical(max_attempts: int = 5):
    """Retry with failure (for critical operations that should not silently fail)"""
    return retry(max_attempts=max_attempts, backoff_factor=1.5, jitter=True, transient=False)