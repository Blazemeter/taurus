# coding=utf-8
from bzt.jmx import JMX
from tests import BZTestCase, RESOURCES_DIR


class TestJMX(BZTestCase):
    def test_jmx_unicode_checkmark(self):
        obj = JMX()
        res = obj._get_http_request("url", "label", "method", 0, {"param": u"✓"}, True)
        prop = res.find(".//stringProp[@name='Argument.value']")
        self.assertNotEqual("BINARY", prop.text)
        self.assertEqual(u"✓", prop.text)

    def test_variable_hostname(self):
        obj = JMX()
        res = obj._get_http_request("http://${hostName}:${Port}/${Path}", "label", "method", 0, {}, True)
        self.assertEqual("/${Path}", res.find(".//stringProp[@name='HTTPSampler.path']").text)
        self.assertEqual("${hostName}", res.find(".//stringProp[@name='HTTPSampler.domain']").text)
        self.assertEqual("${Port}", res.find(".//stringProp[@name='HTTPSampler.port']").text)

    def test_no_port(self):
        obj = JMX()
        res = obj._get_http_request("http://hostname", "label", "method", 0, {}, True)
        self.assertEqual("", res.find(".//stringProp[@name='HTTPSampler.path']").text)
        self.assertEqual("hostname", res.find(".//stringProp[@name='HTTPSampler.domain']").text)
        self.assertEqual("", res.find(".//stringProp[@name='HTTPSampler.port']").text)

    def test_regexp_subject(self):
        res = JMX._get_extractor('test_name', 'baddy', 'regexp', 1, 1, 'error')
        self.assertEqual("body", res.find(".//stringProp[@name='RegexExtractor.useHeaders']").text)
        res = JMX._get_extractor('test_name', 'headers', 'regexp', 1, 1, 'error')
        self.assertEqual("true", res.find(".//stringProp[@name='RegexExtractor.useHeaders']").text)
        res = JMX._get_extractor('test_name', 'http-code', 'regexp', 1, 1, 'error')
        self.assertEqual("code", res.find(".//stringProp[@name='RegexExtractor.useHeaders']").text)
        self.assertEqual("parent", res.find(".//stringProp[@name='Sample.scope']").text)

    def test_int_udv(self):
        res = JMX()
        data = {"varname2": "1", "varname": 1, 2: 3}
        res.add_user_def_vars_elements(data)

    def test_source_ips_single(self):
        obj = JMX()
        res = obj._get_http_request("/", "label", "method", 0, {}, True,
                                    use_random_host_ip=True, host_ips=["192.168.1.1"])
        self.assertEqual("192.168.1.1", res.find(".//stringProp[@name='HTTPSampler.ipSource']").text)

    def test_source_ips_multiple(self):
        obj = JMX()
        res = obj._get_http_request("/", "label", "method", 0, {}, True,
                                    use_random_host_ip=True, host_ips=["192.168.1.1", "192.168.1.2"])
        self.assertEqual("${__chooseRandom(192.168.1.1,192.168.1.2,randomAddr)}",
                         res.find(".//stringProp[@name='HTTPSampler.ipSource']").text)

