from bzt.modules.jmeter import JTLReader
from tests.mocks import EngineEmul
from tests import BZTestCase, local_paths_config, BASE_CONFIG


class TestLoadSamplesReader(BZTestCase):
    HEADER = "timeStamp,elapsed,label,responseCode,responseMessage,threadName,"
    HEADER += "success,bytes,grpThreads,allThreads,Latency,Hostname"
    SAMPLE = "1500000550000,337,http://any-server-in-www.com,200,OK,ping 1-1,true,290418,2,2,65,NameOfHost"

    def setUp(self):
        super(TestLoadSamplesReader, self).setUp()
        self.engine = EngineEmul()
        paths = [BASE_CONFIG, local_paths_config()]
        self.engine.configure(paths)
        self.engine.create_artifacts_dir(paths)
        self.out_file = self.engine.create_artifact('kpi', '.jtl')
        self.obj = JTLReader(self.out_file, self.engine.log, '')
        self.obj.csvreader.read_speed = 1024

    def add_lines(self, line, count):
        with open(self.out_file, 'at', buffering=1) as _file:
            _file.writelines((line + '\n') * count)

    def test_add_results(self):
        # before creation of file
        samples = list(self.obj._read())
        self.assertEqual(len(samples), 0)

        self.add_lines(self.HEADER, 1)
        self.add_lines(self.SAMPLE, 13)

        # read block, rest of file and empty list
        samples = list(self.obj._read())
        self.assertEqual(len(samples), 10)
        samples = list(self.obj._read())
        self.assertEqual(len(samples), 3)
        samples = list(self.obj._read())
        self.assertEqual(len(samples), 0)

    def test_wrong_line(self):
        # test shouldn't stop when wrong sample is encountered
        self.add_lines(self.SAMPLE, 2)
        self.add_lines('wrong-line', 3)
        self.add_lines(self.SAMPLE, 2)
        samples = list(self.obj._read())
        self.assertEqual(len(samples), 2)
