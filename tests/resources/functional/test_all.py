from unittest import TestCase, skipIf


class TestAll(TestCase):
    def test_passing(self):
        pass

    def test_erroring(self):
        raise Exception("Ima broke")

    def test_failing(self):
        self.assertEquals(2 + 2 * 2, 8)

    @skipIf(2 > 1, "Skip everytime")
    def test_skipped(self):
        pass
