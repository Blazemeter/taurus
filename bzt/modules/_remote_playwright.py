"""
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
import os

from bzt import TaurusConfigError
from bzt.modules import RemoteExecutor
from bzt.modules._bridge_file_puller import BridgeFilePuller
from bzt.modules.javascript import PlaywrightLogReader, PlaywrightFuncReader

JSONL_FILENAME = "taurus-playwright-reporter.jsonl"


class RemotePlaywrightExecutor(RemoteExecutor):
    """
    Runs Playwright tests on a remote host via the Taurus Bridge,
    following the RemotePyTestExecutor pattern.
    """

    def __init__(self):
        super(RemotePlaywrightExecutor, self).__init__()
        self._pullers = []
        self.remote_report_path = None

    def create_load_reader(self, report_file):
        return PlaywrightLogReader(report_file, self.log)

    def create_func_reader(self, report_file):
        return PlaywrightFuncReader(report_file, self.engine, self.log)

    def prepare(self):
        self.log.info("Starting RemotePlaywrightExecutor")
        super(RemotePlaywrightExecutor, self).prepare()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for playwright executor")

        self.remote_report_path = self.remote_artifacts_path + '/' + JSONL_FILENAME
        self.reporting_setup(suffix=".jsonl")

    def startup(self):
        # upload the test script
        remote_script_path = self.remote_artifacts_path + '/' + os.path.basename(self.script)
        self.upload_file(self.script, remote_script_path)

        # upload playwright.config.ts if it exists alongside the script
        script_dir = os.path.dirname(self.script)
        for config_name in ("playwright.config.ts", "playwright.config.js"):
            config_path = os.path.join(script_dir, config_name)
            if os.path.exists(config_path):
                remote_config = self.remote_artifacts_path + '/' + config_name
                self.upload_file(config_path, remote_config)

        # compute load parameters (same logic as PlaywrightTester.startup())
        load = self.get_load()
        concurrency = max(1, load.concurrency or 1)

        if load.duration > 0:
            if load.iterations > 0:
                repeat_each = concurrency * load.iterations
            else:
                repeat_each = 1000
        else:
            iterations = max(1, load.iterations or 1)
            repeat_each = concurrency * iterations

        reporter = "@taurus/playwright-custom-reporter"
        remote_output = self.remote_artifacts_path + '/test-output'

        # build env vars for the reporter
        env_parts = [
            'TAURUS_PWREPORT_STDOUT=true',
            'TAURUS_PWREPORT_DIR=' + self.remote_artifacts_path,
        ]
        if load.duration > 0:
            env_parts.append('TAURUS_PWREPORT_DURATION=' + str(int(load.duration * 1000)))

        # build the playwright command
        cmdline = 'npx playwright test'
        cmdline += ' --reporter "' + reporter + '"'
        cmdline += ' --workers ' + str(concurrency)
        cmdline += ' --repeat-each ' + str(repeat_each)
        cmdline += ' --output ' + remote_output

        # prepend env vars (works on Linux where Charmander runs)
        env_prefix = ' '.join(env_parts)
        full_cmd = env_prefix + ' ' + cmdline

        self.runner_pid = self.command(
            full_cmd,
            wait_for_completion=False,
            workingDir=self.remote_artifacts_path
        ).get('pid')

        # start pulling the JSONL report file
        p = BridgeFilePuller(
            file_url=self.file_url,
            remote_path=self.remote_report_path,
            local_path=self.report_file,
            log=self.log,
        )
        p.start()
        self._pullers.append(p)

    def check(self):
        # Override base RemoteExecutor.check() which uses Windows tasklist.
        # Charmander runs on Linux — use ps to check if PID is still running.
        if self.runner_pid != 0:
            result = self.command('ps -p ' + str(self.runner_pid) + ' -o pid=')
            output = result.get('output', '').strip()
            if str(self.runner_pid) not in output:
                return True
            return False
        return True

    def has_results(self):
        if not self.reader:
            return False
        if hasattr(self.reader, 'read_records'):
            return bool(self.reader.read_records)
        return True

    def shutdown(self):
        for p in self._pullers:
            p.stop()
        # Override base RemoteExecutor.shutdown() which uses Windows taskkill.
        # Charmander runs on Linux — use kill.
        self.log.info("Terminating remote process with PID %s", self.runner_pid)
        self.command('kill -9 ' + str(self.runner_pid))

    def post_process(self):
        super(RemotePlaywrightExecutor, self).post_process()
