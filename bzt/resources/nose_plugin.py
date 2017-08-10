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
import json
import sys
import time
import traceback
from optparse import OptionParser

from bzt.modules.python import ApiritifSampleExtractor, Sample
import nose
from nose.plugins import Plugin


def get_apiritif():
    try:
        import apiritif
    except ImportError:
        apiritif = None
    return apiritif


class BZTPlugin(Plugin):
    """
    Saves test results in a format suitable for Taurus.
    """

    name = 'bzt_plugin'
    enabled = True

    def __init__(self, output_file):
        super(BZTPlugin, self).__init__()
        self.output_file = output_file
        self.test_count = 0
        self.success_count = 0
        self.current_sample = None
        self.out_stream = None
        self.apiritif_extractor = ApiritifSampleExtractor()

    def __enter__(self):
        self.out_stream = open(self.output_file, "wt", buffering=1)
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.out_stream.close()

    def begin(self):
        """
        Before any test runs
        open descriptor here
        :return:
        """
        pass

    def finalize(self, result):
        """
        After all tests
        :param result:
        :return:
        """
        del result
        if not self.test_count:
            raise RuntimeError("Nothing to test.")

    def startTest(self, test):  # pylint: disable=invalid-name
        """
        before test run
        :param test:
        :return:
        """
        test_file, _, _ = test.address()  # file path, module name, class.method
        test_fqn = test.id()  # [package].module.class.method
        class_name, method_name = test_fqn.split('.')[-2:]

        self.current_sample = Sample(test_case=method_name,
                                     test_suite=class_name,
                                     start_time=time.time(),
                                     status="SKIPPED")
        self.current_sample.extras.update({
            "file": test_file,
            "full_name": test_fqn,
            "description": test.shortDescription()
        })

    def addError(self, test, error):  # pylint: disable=invalid-name
        """
        when a test raises an uncaught exception
        :param test:
        :param error:
        :return:
        """
        # test_dict will be None if startTest wasn't called (i.e. exception in setUp/setUpClass)
        if self.current_sample is not None:
            self.current_sample.status = "BROKEN"
            self.current_sample.error_msg = str(error[1]).split('\n')[0]
            self.current_sample.error_trace = self._get_trace(error)

    @staticmethod
    def _get_trace(error):
        if sys.version > '3':
            lines = traceback.format_exception(*error, chain=not isinstance(error[1], str))
        else:
            lines = traceback.format_exception(*error)
        return ''.join(lines).rstrip()

    def addFailure(self, test, error):  # pylint: disable=invalid-name
        """
        when a test fails
        :param test:
        :param error:

        :return:
        """
        self.current_sample.status = "FAILED"
        self.current_sample.error_msg = str(error[1]).split('\n')[0]
        self.current_sample.error_trace = self._get_trace(error)

    def addSkip(self, test):  # pylint: disable=invalid-name
        """
        when a test is skipped
        :param test:
        :return:
        """
        self.current_sample.status = "SKIPPED"

    def addSuccess(self, test):  # pylint: disable=invalid-name
        """
        when a test passes
        :param test:
        :return:
        """
        self.current_sample.status = "PASSED"
        self.success_count += 1

    def process_apiritif_samples(self, sample):
        samples_processed = 0
        apiritif = get_apiritif()
        test_case = sample.test_case

        recording = apiritif.recorder.get_recording(test_case)
        if not recording:
            return samples_processed

        samples = self.apiritif_extractor.parse_recording(recording, sample)
        for sample in samples:
            samples_processed += 1
            self.test_count += 1
            self.write_sample(sample)
            self.write_stdout_report(sample.test_case)

        return samples_processed

    def process_sample(self, sample):
        self.test_count += 1
        self.write_sample(sample)
        self.write_stdout_report(sample.test_case)

    def write_sample(self, sample):
        self.out_stream.write("%s\n" % json.dumps(sample.to_dict()))
        self.out_stream.flush()

    def write_stdout_report(self, label):
        report_pattern = "%s,Total:%d Passed:%d Failed:%d\n"
        failed = self.test_count - self.success_count
        sys.stdout.write(report_pattern % (label, self.test_count, self.success_count, failed))
        sys.stdout.flush()

    def stopTest(self, test):  # pylint: disable=invalid-name
        """
        after the test has been run
        :param test:
        :return:
        """
        self.current_sample.duration = time.time() - self.current_sample.start_time

        if get_apiritif() is not None:
            samples_processed = self.process_apiritif_samples(self.current_sample)
            if samples_processed == 0:
                self.process_sample(self.current_sample)
        else:
            self.process_sample(self.current_sample)

        self.current_sample = None


def run_nose(report_file, files, iteration_limit, hold):
    argv = [__file__, '-v']
    argv.extend(files)
    argv.extend(['--with-bzt_plugin', '--nocapture', '--exe', '--nologcapture'])

    if iteration_limit == 0:
        if hold > 0:
            iteration_limit = sys.maxsize
        else:
            iteration_limit = 1

    start_time = int(time.time())
    with BZTPlugin(report_file) as plugin:
        iteration = 0
        while True:
            nose.run(addplugins=[plugin], argv=argv)
            iteration += 1
            if 0 < hold < int(time.time()) - start_time:
                break
            if iteration >= iteration_limit:
                break


if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option('-r', '--report-file', action='store', default='nose_report.ldjson')
    parser.add_option('-i', '--iterations', action='store', default=0)
    parser.add_option('-d', '--duration', action='store', default=0)
    parser.add_option('-w', '--with-nose_plugin', action='store', default=0)
    opts, args = parser.parse_args()

    run_nose(opts.report_file, args, int(opts.iterations), float(opts.duration))
