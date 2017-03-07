import signal
import bzt.utils
bzt.utils.is_piped = lambda _: False

from cli import signal_handler, main

signal.signal(signal.SIGINT, signal_handler)
signal.signal(signal.SIGTERM, signal_handler)
main()