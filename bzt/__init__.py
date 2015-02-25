"""
Some meta-info and simple classes
"""

version = "0.1.2"

import signal


def signal_handler(sig, frame):
    """
    required for non-tty python runs to interrupt
    :param frame:
    :param sig:
    """
    raise ManualShutdown()


signal.signal(signal.SIGINT, signal_handler)
signal.signal(signal.SIGTERM, signal_handler)


class NormalShutdown(KeyboardInterrupt):
    pass


class ManualShutdown(KeyboardInterrupt):
    pass


class AutomatedShutdown(KeyboardInterrupt):
    pass


