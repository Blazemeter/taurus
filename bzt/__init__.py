"""
Copyright 2015 BlazeMeter Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""

version = "0.2.10"

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


class RCProvider(object):
    def get_rc(self):
        raise NotImplementedError()


class NormalShutdown(KeyboardInterrupt, RCProvider):
    def get_rc(self):
        return 0


class ManualShutdown(KeyboardInterrupt, RCProvider):
    def get_rc(self):
        return 2


class AutomatedShutdown(KeyboardInterrupt, RCProvider):
    def get_rc(self):
        return 3


