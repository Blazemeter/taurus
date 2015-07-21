import logging
import os
from bzt.six import etree
import traceback
import yaml
# from collections import OrderedDict

KNOWN_TAGS = ["hashTree", "jmeterTestPlan", "TestPlan", "ResultCollector",
              "HTTPSamplerProxy",
              "ThreadGroup",
              "kg.apc.jmeter.timers.VariableThroughputTimer",
              "kg.apc.jmeter.threads.SteppingThreadGroup",
              "DNSCacheManager",
              "HeaderManager",
              "CacheManager",
              "CookieManager",
              "ConfigTestElement",
              "DurationAssertion",
              "ConstantTimer",
              "RegexExtractor",
              "HtmlExtractor",
              "com.atlantbh.jmeter.plugins.jsonutils.jsonpathextractor.JSONPathExtractor",
              "com.atlantbh.jmeter.plugins.jsonutils.jsonpathassertion.JSONPathAssertion",
              "ResponseAssertion",
              "CSVDataSet",
              "GenericController",
              "ResultCollector"]


class Converter(object):
    def __init__(self, jmx_file):
        self.log = logging.getLogger(self.__class__.__name__)
        self.xml_tree = None
        self.jmx_file = os.path.abspath(os.path.expanduser(jmx_file))
        self.global_objects = []
        self.scenario = {"execution": None, "scenarios": None}

    def load_jmx(self):
        """
        Load jmx file as lxml etree
        :return:
        """
        self.log.debug('Loading jmx file %s', self.jmx_file)

        if os.path.exists(self.jmx_file):
            try:
                self.tree = etree.fromstring(open(self.jmx_file, "rb").read())
            except BaseException as exc:
                self.log.error("Error while loading jmx file: %s", traceback.format_exc())
        else:
            self.log.error("File %s does not exist", self.jmx_file)

    def request_get_url(self, http_sampler_element):
        """
        Converts to dict
        :return:
        """
        sampler_dict = self.convert_etree_to_dict(http_sampler_element,
                                                  {"protocol": "HTTPSampler.protocol", "domain": "HTTPSampler.domain"})
        url = "http" + "://" + sampler_dict["domain"] + "/"
        return url

    def get_request_body(self, http_sampler_element):
        """
        Get body params from sampler
        :param http_sampler_element:
        :return: dict
        """
        raw_body = http_sampler_element.find(".//boolProp[@name='HTTPSampler.postBodyRaw']")

        if raw_body is not None and raw_body.text == "true":
            http_args_element = http_sampler_element.find(".//elementProp").find(".//collectionProp").find(
                ".//elementProp")
            body = http_args_element.find(".//stringProp[@name='Argument.value']").text
            return {"body": body}
        else:
            body_params = {}
            http_args_collection = http_sampler_element.find(".//elementProp").find(".//collectionProp").findall(
                ".//elementProp")
            for element in http_args_collection:
                body_params[element.get("name")] = element.find(".//stringProp[@name='Argument.value']").text
            return {"body": body_params}

    def get_request_headers(self):
        """
        Get local request headers
        :return:
        """

    def convert_etree_to_dict(self, etree_element, params):
        result_dict = {}
        for param, value in params.items():
            val = etree_element.find("*[@name=" + "'" + value + "'" + "]").text
            if val:
                result_dict[param] = val
                # else:
                #     result_dict[param] = ""
        return result_dict

    def get_thread_groups(self):
        """
        Get all thread groups from jmx, convert to dict.
        :return: dict
        """

        self.log.debug("Processing thread groups...")
        tg_elements = self.tree.findall(".//ThreadGroup")
        if tg_elements:
            self.log.debug("Total thread groups: %d", len(tg_elements))
            for tg_element in tg_elements:
                ht_element = tg_element.getnext()
                urls = []
                tg_dict = {"scenarios": {tg_element.get("testname"): {"requests": urls}}}
                self.scenario["execution"] = {"scenario": tg_element.get("testname")}
                if ht_element.tag == "hashTree":
                    request_elements = ht_element.findall(".//HTTPSamplerProxy")
                    self.log.debug("Total http samplers in tg groups: %d", len(request_elements))
                    for request_element in request_elements:
                        self.log.debug("Processing HTTPSamplerProxy %s in tg %s", tg_element.get("testname"),
                                       request_element.get("testname"))

                        request_config = {}
                        request_config.update({"url": self.request_get_url(request_element)})
                        request_config.update(self.get_request(request_element))
                        request_config.update({"label": request_config["url"]})
                        request_config.update(self.get_request_body(request_element))
                        urls.append(request_config)
                        self.scenario.update(tg_dict)
        else:
            self.log.warning("No thread groups was found!")

    def get_global_objects(self):
        """
        Get global elements
        :return:
        """

    def get_request(self, element):
        """
        returns request dict
        :param element:
        :return:
        """
        fields = {"method": "HTTPSampler.method"}
        return self.convert_etree_to_dict(element, fields)

    def convert(self):
        """
        Converts all
        :return:
        """
        tg_dict = self.get_thread_groups()

    def check_if_disabled(self, element):
        """
        Returns True if any parent element is disabled
        :return:
        """
        parent_disabled = False
        parent = element.getparent()

        while parent is not None:
            if parent.get('enabled') == 'false':
                parent_disabled = True
                break
            parent = parent.getparent()

        return parent_disabled

    def get_depth(self, element):
        return len([ancestor for ancestor in element.iterancestors()])

    def remove_element(self, element):
        sibling = element.getnext()
        if sibling is not None and sibling.tag == "hashTree":
            # self.log.debug("Removing hashtree %s, %s", sibling.tag, sibling.get("name"))
            sibling.getparent().remove(sibling)

        element.getparent().remove(element)

    def clean_disabled_elements(self, element):

        for subelement in element.iter():
            # self.log.debug("subelement %s %s %s %d %s", subelement.tag, subelement.get("name", ""),
            #                subelement.get("testclass", ""), self.get_depth(subelement), subelement.text)
            if subelement.tag.endswith("prop"):
                continue
            if subelement.get("enabled") == 'false':
                self.log.debug("Removing disabled element %s, %s", element.tag, element.get("name"))
                self.remove_element(subelement)
                self.clean_disabled_elements(element)
                return

    def clean_jmx_tree(self, element):
        """
        Purge disabled and unknown elements from etree
        :return:
        """
        for subelement in element.iter():
            # self.log.debug("subelement %s %s %s %d %s", subelement.tag, subelement.get("name", ""),
            #                    subelement.get("testclass", ""), self.get_depth(subelement), subelement.text)
            if subelement.tag.lower().endswith("prop"):
                continue

            if subelement.tag not in KNOWN_TAGS:
                self.log.debug("Removing unknown element: %s", subelement.tag)
                self.remove_element(subelement)
                self.clean_jmx_tree(element)
                return

    def dump_yaml(self):
        with open("/home/alex/tmp/disabled.yml", "wt") as fds:
            yaml.dump(self.scenario, fds, default_flow_style=False)


def main():
    pass
