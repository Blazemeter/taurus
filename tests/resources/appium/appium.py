# Mock for appium
# it supports next functional:
# 1. exit successfully if 'list' command received.
# 2. wait for terminate if no parameters are sent

import sys
import time

if len(sys.argv) == 2 and sys.argv[1] == '--version':
    print('It is fake appium')
    exit(0)
elif len(sys.argv) == 1:
    time.sleep(100)
    exit(1)
else:
    exit(1)
