"""
EFT (Exclude Failed Transactions) helper for browser-based (Selenium/Apiritif) tests.

Produces a generic ``failed_transactions.json`` artifact that categorizes per-transaction
failures into general errors vs. failed assertions vs. failed embedded resources, mirroring
the existing three-way error split used for JMeter results.

Design references:
    - ``error_item_skel`` shape: ``bzt/modules/aggregator.py`` (KPISet.error_item_skel,
      keys: cnt/msg/tag/rc/type/urls/responseBodies).
    - ERRTYPE_* classification: extracted (NOT imported-and-mutated) from
      ``JTLErrorsReader.find_failure`` in ``bzt/modules/jmeter.py``. ``jmeter.py`` stays
      UNCHANGED per FR-008; only the classification semantics are re-expressed here for the
      executor-agnostic Selenium path.
    - Three-way split (errors/assertions/failedEmbeddedResources) mirrors
      ``DatapointSerializer.__add_errors`` in ``bzt/modules/blazemeter/blazemeter_reporter.py``.

Public surface (used by BlazeMeterUploader.post_process):
    classify_failure(label, message, trace, rc, assertion_name) -> error_item_skel dict
    recover_assertion_name(sample) -> str | None
    build_failed_transactions(transactions, session_id, test_id=None, timestamp=None) -> dict

Traces to specs/mob-43135/spec.md FR-002, FR-003, FR-004, FR-005 and
specs/mob-43135/contracts/failed_transactions.schema.json.
"""
from collections import Counter

from bzt.modules.aggregator import KPISet

# Marker used to detect an assertion failure in a functional sample's error text.
# Apiritif/Selenium assertion failures surface as a Python ``AssertionError`` (e.g.
# apiritif's ``assert_in_body`` raises a bare ``AssertionError`` — see
# ``bzt/modules/_apiritif/generator.py::_gen_assertions``). General browser errors
# (e.g. ``NoSuchElementException``) do NOT carry this marker.
_ASSERTION_MARKER = "AssertionError"

# Prefix for a synthetic assertion name derived from the transaction label when the real
# assertion name is not recoverable from the sample's error_msg/error_trace (FR-004).
_SYNTHETIC_ASSERT_PREFIX = "assert::"


def _looks_like_assertion(message, trace):
    """Return True if the failure text indicates a raised assertion (vs a general error).

    Mirrors the intent of ``JTLErrorsReader.find_failure`` where an assertion failure is
    distinguished from a response/general error before being typed ERRTYPE_ASSERT. For the
    functional (Selenium/Apiritif) path there is no JTL ``<assertionResult>`` element, so the
    signal is the presence of an ``AssertionError`` in the error message or stacktrace.
    """
    for text in (message, trace):
        if text and _ASSERTION_MARKER in text:
            return True
    return False


def recover_assertion_name(sample):
    """Recover the assertion name for a failed FunctionalSample, or None.

    A-ASSERT (VALIDATE): assertion names for Selenium/Apiritif failures were assumed to be
    recoverable from ``FunctionalSample.error_msg``/``error_trace``. The contract probe
    (tests/unit/modules/test_eft_assertion_probe.py) proves that for the real apiritif
    ``assert_in_body`` failure shape the name is NOT separably recoverable — the error is a
    bare ``AssertionError`` with no distinguishable name token. So:

    - For a genuine assertion failure with no recoverable name, synthesize
      ``assert::<test_case>`` from the transaction label (FR-004 graceful degradation).
    - For a non-assertion (general) failure — e.g. ``NoSuchElementException`` — return None.
      The caller (classify_failure) is responsible for typing the item; this function must
      NOT fabricate an assertion name for every failure.

    :type sample: bzt.modules.functional.FunctionalSample
    :rtype: str | None
    """
    message = getattr(sample, "error_msg", None)
    trace = getattr(sample, "error_trace", None)

    if not _looks_like_assertion(message, trace):
        return None

    # No separable real name is recoverable for the proven apiritif shape — synthesize one
    # from the transaction label (the test-case / method name).
    label = getattr(sample, "test_case", None) or getattr(sample, "test_suite", None) or ""
    return "%s%s" % (_SYNTHETIC_ASSERT_PREFIX, label)


def classify_failure(label, message, trace, rc, assertion_name):
    """Map a single browser-test failure to an ``error_item_skel``-shaped dict.

    Classification (extracted from the ERRTYPE_* semantics in
    ``JTLErrorsReader.find_failure``):
    - When an ``assertion_name`` is supplied, the failure is typed ERRTYPE_ASSERT and the
      name is carried in ``tag``.
    - Otherwise the failure is a general error, typed ERRTYPE_ERROR with ``tag`` None.

    Graceful degradation (FR-005): ``rc`` is tolerated as None (browser errors have no HTTP
    response code) and ``responseBodies`` is always empty for browser tests (no JMeter-style
    response bodies are captured).

    :param label: transaction label (used for context/logging only)
    :param message: one-line error message (may be None)
    :param trace: error stacktrace (may be None)
    :param rc: response code string or None
    :param assertion_name: recovered/synthetic assertion name, or None for general errors
    :rtype: dict
    """
    if assertion_name:
        err_type = KPISet.ERRTYPE_ASSERT
        tag = assertion_name
    else:
        err_type = KPISet.ERRTYPE_ERROR
        tag = None

    # Shape mirrors KPISet.error_item_skel (aggregator.py). responseBodies is empty and rc is
    # tolerated as None for browser tests (FR-005).
    return {
        "cnt": 1,
        "msg": message or "",
        "tag": tag,
        "rc": rc,
        "type": err_type,
        "urls": Counter(),
        "responseBodies": [],
    }


def build_failed_transactions(transactions, session_id, test_id=None, timestamp=None):
    """Assemble the ``failed_transactions.json`` artifact structure.

    Mirrors the ``DatapointSerializer.__add_errors`` three-way split
    (errors/assertions/failedEmbeddedResources) at the per-transaction level.

    :param transactions: list of per-transaction dicts already carrying the three-way split
        (keys: label, timestamp, duration, errors, assertions, failedEmbeddedResources)
    :param session_id: BlazeMeter session id (may be None)
    :param test_id: BlazeMeter test id (may be None)
    :param timestamp: report timestamp epoch seconds (may be None)
    :rtype: dict
    """
    report_transactions = []
    for txn in transactions:
        report_transactions.append({
            "label": txn.get("label"),
            "timestamp": txn.get("timestamp"),
            "duration": txn.get("duration"),
            "errors": list(txn.get("errors", [])),
            "assertions": list(txn.get("assertions", [])),
            "failedEmbeddedResources": list(txn.get("failedEmbeddedResources", [])),
        })

    return {
        "reportInfo": {
            "sessionId": session_id,
            "testId": test_id,
            "timestamp": timestamp,
            "type": "FAILED_TRANSACTIONS",
        },
        "transactions": report_transactions,
    }
