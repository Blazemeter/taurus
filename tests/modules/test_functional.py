from tests import BZTestCase



# TODO: test ReportReader, Aggregation, ConsoleInterface
class TestReportReader(BZTestCase):
    def test_read_ldjson(self):
        raise NotImplementedError()

    def test_parse_entries(self):
        raise NotImplementedError()


class TestConsoleReporter(BZTestCase):
    def test_with_data(self):
        raise NotImplementedError()


class TestFinalStats(BZTestCase):
    def test_works(self):
        raise NotImplementedError()

    def test_print_stacktrace(self):
        raise NotImplementedError()
