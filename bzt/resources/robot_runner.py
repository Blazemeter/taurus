import json
import sys
import time
from datetime import datetime
from optparse import OptionParser

from apiritif.samples import Sample
from io import StringIO
from robot import run


class TaurusListener:
    ROBOT_LISTENER_API_VERSION = 2

    def __init__(self, report_filename):
        self._report_filename = report_filename
        self._report_file = None
        self._test_count = 0
        self._passed_tests = 0
        self._failed_tests = 0
        self._current_suite = None

    def prepare(self):
        self._report_file = open(self._report_filename, 'wt')

    def post_process(self):
        if self._report_file is not None:
            self._report_file.close()

    def get_test_count(self):
        return self._test_count

    def _report_sample(self, sample):
        self._test_count += 1
        if sample.status == "PASSED":
            self._passed_tests += 1
        elif sample.status in ["BROKEN", "FAILED"]:
            self._failed_tests += 1

        self._write_sample(sample)
        self._write_stdout_report(sample.test_case)

    def _write_sample(self, sample):
        if self._report_file is None:
            raise ValueError("Plugin wasn't prepared")

        self._report_file.write("%s\n" % json.dumps(sample.to_dict()))
        self._report_file.flush()

    def _write_stdout_report(self, label):
        report_pattern = "%s,Total:%d Passed:%d Failed:%d\n"
        sys.stdout.write(report_pattern % (label, self._test_count, self._passed_tests, self._failed_tests))
        sys.stdout.flush()

    def start_suite(self, name, attrs):
        self._current_suite = name

    def start_test(self, name, attrs):
        pass

    def end_test(self, name, attrs):
        # TODO: include keywords as subsamples
        sample = Sample()
        sample.test_case = name
        sample.test_suite = self._current_suite
        sample.start_time = time.mktime(datetime.strptime(attrs['starttime'], '%Y%m%d %H:%M:%S.%f').timetuple())
        sample.duration = float(attrs['elapsedtime']) / 1000.0
        if attrs['status'] == 'PASS':
            sample.status = 'PASSED'
        else:
            sample.status = 'FAILED'
            message = attrs['message']
            if '\n' in message:
                lines = message.split('\n')
                sample.error_msg = lines[0]
                sample.error_trace = message
            else:
                sample.error_msg = message
        self._report_sample(sample)

    def end_suite(self, name, attrs):
        self._current_suite = None


def run_robot(targets, report_file, iteration_limit, duration_limit, variablefile, outputfile, logfile, include, cmdline):
    listener = TaurusListener(report_file)
    listener.prepare()
    stdout = StringIO()
    stderr = StringIO()
    start_time = int(time.time())
    iteration = 0
    try:
        while True:
            kwargs = {
                'listener': listener,  # pass Taurus listener
                'output': outputfile, 'log': logfile, 'report': None,
                'stdout': stdout, 'stderr': stderr,  # capture stdout/stderr
            }
            if variablefile is not None:
                kwargs['variablefile'] = variablefile
            if include is not None:
                kwargs['include'] = include
            if cmdline:
                parts = cmdline[1:-1].split(" ")
                part = iter(parts)
                kwargs.update(dict(zip(part, part)))

            run(*targets, **kwargs)
            iteration += 1
            if 0 < duration_limit < int(time.time()) - start_time:
                break
            if iteration >= iteration_limit:
                break
    finally:
        listener.post_process()

    sys.stdout.write("Robot stdout:\n" + stdout.getvalue() + "\n")
    sys.stderr.write("Robot stderr:\n" + stderr.getvalue() + "\n")

    if listener.get_test_count() == 0:
        raise ValueError("Nothing to test. No tests were found.")


if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option('-r', '--report-file', action='store', default='report.ldjson')
    parser.add_option('-i', '--iterations', action='store', default=0)
    parser.add_option('-d', '--duration', action='store', default=0)
    parser.add_option('-v', '--variablefile', action='store', default=None)
    parser.add_option('-o', '--outputfile', action='store', default=None)
    parser.add_option('-l', '--logfile', action='store', default=None)
    parser.add_option('--include', action='store', default=None)
    parser.add_option('--cmdline', action='store', default=None)
    opts, args = parser.parse_args()
    if opts.include is not None:
        opts.include = opts.include.split(',')
    opts.iterations = int(opts.iterations)
    opts.duration = float(opts.duration)

    if opts.iterations == 0:
        if opts.duration > 0:
            opts.iterations = sys.maxsize
        else:
            opts.iterations = 1

    run_robot(args, opts.report_file, int(opts.iterations), float(opts.duration), opts.variablefile,
              opts.outputfile, opts.logfile, opts.include, opts.cmdline)
