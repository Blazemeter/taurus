"""
Module for reporting into http://www.blazemeter.com/ service

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

from bzt.engine import Service
from bzt.modules.blazemeter.cloud_provisioning import CloudProvisioning


class ServiceStubScreenshoter(Service):
    def startup(self):
        if not isinstance(self.engine.provisioning, CloudProvisioning):
            self.log.warning("Stub for service 'screenshoter', use cloud provisioning to have it working")


class ServiceStubCaptureHAR(Service):
    def startup(self):
        if not isinstance(self.engine.provisioning, CloudProvisioning):
            self.log.warning("Stub for service 'capturehar', use cloud provisioning to have it working")
