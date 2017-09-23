import json

from bzt.modules.functional import LoadSamplesReader
from tests.mocks import EngineEmul
from tests import BZTestCase, local_paths_config, BASE_CONFIG


class TestLoadSamplesReader(BZTestCase):
    SAMPLE = json.dumps({
        "status": "PASSED",
        "duration": 0.264,
        "test_case": "http://blazedemo.com",
        "start_time": 1500000980.140341,
        "error_msg": None,
        "content": "lorem"})

    def setUp(self):
        super(TestLoadSamplesReader, self).setUp()
        self.engine = EngineEmul()
        paths = [BASE_CONFIG, local_paths_config()]
        self.engine.configure(paths)
        self.engine.create_artifacts_dir(paths)
        self.out_file = self.engine.create_artifact('NoseTester', '.ldjson')
        self.obj = LoadSamplesReader(self.out_file, self.engine.log)
        self.obj.report_reader.json_reader.block_size = 1024

    def add_lines(self, line, count):
        with open(self.out_file, 'at', buffering=1) as _file:
            _file.writelines((line + '\n') * count)

    def test_add_results(self):
        # before creation of file
        samples = list(self.obj._read())
        self.assertEqual(len(samples), 0)

        self.add_lines(self.SAMPLE, 10)

        # read block, rest of file and empty list
        samples = list(self.obj._read())
        self.assertEqual(len(samples), 7)
        samples = list(self.obj._read())
        self.assertEqual(len(samples), 3)
        samples = list(self.obj._read())
        self.assertEqual(len(samples), 0)

        # add results
        self.add_lines(self.SAMPLE, 2)

        # read additions
        samples = list(self.obj._read())
        self.assertEqual(len(samples), 2)
        self.add_lines(self.SAMPLE, 3)
        samples = list(self.obj._read(last_pass=True))
        self.assertEqual(len(samples), 3)

    def test_wrong_line(self):
        # test shouldn't stop when wrong sample is encountered
        self.add_lines(self.SAMPLE, 2)
        self.add_lines('wrong-line', 3)
        self.add_lines(self.SAMPLE, 2)
        samples = list(self.obj._read())
        self.assertEqual(len(samples), 2)




