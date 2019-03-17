import unittest, yaml


class MyTestCase(unittest.TestCase):
    def test_case(self):
        data = {"str": "\tpart1\tpart2\t"}
        res = yaml.safe_dump(data,
                             default_flow_style=False, explicit_start=True, canonical=False,
                             allow_unicode=True, encoding='utf-8', width=float("inf"))
        self.assertEqual('---\nstr: "\\tpart1\\tpart2\\t\"\n', res)
