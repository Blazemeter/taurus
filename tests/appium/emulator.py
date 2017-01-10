# Mock for emulator
# it supports next functional:
# 1. wait for terminate if no 2 parameters are sent

import sys
import time

if len(sys.argv) == 3 and sys.argv[1] == '-avd':
    print('wait for terminating...')
    time.sleep(100)
    exit(1)
else:
    exit(1)
