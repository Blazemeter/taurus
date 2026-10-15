"""
Helpers for the 'skip-on-failure' scenario option of generated Selenium scripts.
No selenium imports here: generated scripts get it through selenium_extras, unit tests import it directly.

Copyright 2026 BlazeMeter Inc.

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
import logging

from apiritif import get_from_thread_store, get_iteration
from apiritif.action_plugins import BaseActionHandler

SKIPPED = "skipped"

log = logging.getLogger(__name__)


def report_skipped_steps(steps):
    """
    Report steps left out by 'skip-on-failure' as skipped test cases.

    Goes to action handlers only: no flow marker (no grid round-trip) and no sample,
    so skipped steps never count as hits or errors.

    :param steps: remaining (method, label) pairs, label is None for non-transaction steps
    """
    labels = [label for _, label in steps if label is not None]
    if not labels:
        return

    test_suite, scenario_name, data_sources, action_handlers, driver = get_from_thread_store(
        ['test_suite', 'scenario_name', 'data_sources', 'action_handlers', 'driver'])
    session_id = driver.session_id if driver else None

    for label in labels:
        start_params = {"testCaseName": label, "testSuiteName": scenario_name or test_suite}
        if data_sources:
            start_params["testDataIterationId"] = get_iteration()
        stop_params = {"status": SKIPPED, "message": ""}

        for handler in action_handlers or []:
            try:
                handler.handle(session_id, BaseActionHandler.TEST_CASE_START, start_params)
                handler.handle(session_id, BaseActionHandler.TEST_CASE_STOP, stop_params)
            except Exception as exc:  # reporting must never break the iteration
                log.warning("Failed to report skipped step '%s': %s", label, exc)
