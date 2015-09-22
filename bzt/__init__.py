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
from abc import abstractmethod
import os
import sys
import signal

VERSION = "0.4.4"


def signal_handler(sig, frame):
    """
    required for non-tty python runs to interrupt
    :param frame:
    :param sig:
    """
    del sig, frame
    raise ManualShutdown()


signal.signal(signal.SIGINT, signal_handler)
signal.signal(signal.SIGTERM, signal_handler)


class RCProvider(object):
    """
    Abstract return code provider
    """

    @abstractmethod
    def get_rc(self):
        """
        Must be implemented in subclasses
        """
        pass


class NormalShutdown(KeyboardInterrupt, RCProvider):
    def get_rc(self):
        """
        Returns normal rc
        :return: int
        """
        return 0


class ManualShutdown(KeyboardInterrupt, RCProvider):
    def get_rc(self):
        """
        Returns manual shutdown rc
        :return: int
        """
        return 2


class AutomatedShutdown(KeyboardInterrupt, RCProvider):
    def get_rc(self):
        """
        Returns automated shutdown rc
        :return: int
        """
        return 3


def get_configs_dir():
    """
    Generate configs dir path on install, moved from utils due to import error
    :return: str
    """
    path = os.getenv("VIRTUAL_ENV", "") \
        if os.getenv("VIRTUAL_ENV", "") \
        else os.path.splitdrive(sys.executable)[0]
    path += os.path.sep + os.path.join("etc", "bzt.d")  # os.path.join does not work for some reason
    return path
