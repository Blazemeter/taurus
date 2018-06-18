from lxml import etree

from bzt import TaurusConfigError
from bzt.engine import Scenario
from bzt.jmx import JMX
from bzt.jmx.tools import ProtocolHandler
from bzt.utils import ensure_is_dict


class AssertionsHandler(ProtocolHandler):
    def get_processor_elements(self, scenario, request):
        elements = []

        assertions = request.config.get("assert", [])
        for idx, assertion in enumerate(assertions):
            assertion = ensure_is_dict(assertions, idx, "contains")
            if not isinstance(assertion['contains'], list):
                assertion['contains'] = [assertion['contains']]
            elements.append(JMX._get_resp_assertion(assertion.get("subject", Scenario.FIELD_BODY),
                                                    assertion['contains'],
                                                    assertion.get('regexp', True),
                                                    assertion.get('not', False),
                                                    assertion.get('assume-success', False)))
            elements.append(etree.Element("hashTree"))

        jpath_assertions = request.config.get("assert-jsonpath", [])
        for idx, assertion in enumerate(jpath_assertions):
            assertion = ensure_is_dict(jpath_assertions, idx, "jsonpath")

            exc = TaurusConfigError('JSON Path not found in assertion: %s' % assertion)
            component = JMX._get_json_path_assertion(assertion.get('jsonpath', exc),
                                                     assertion.get('expected-value', ''),
                                                     assertion.get('validate', False),
                                                     assertion.get('expect-null', False),
                                                     assertion.get('invert', False),
                                                     assertion.get('regexp', True))
            elements.append(component)
            elements.append(etree.Element("hashTree"))

        xpath_assertions = request.config.get("assert-xpath", [])
        for idx, assertion in enumerate(xpath_assertions):
            assertion = ensure_is_dict(xpath_assertions, idx, "xpath")

            exc = TaurusConfigError('XPath not found in assertion: %s' % assertion)
            component = JMX._get_xpath_assertion(assertion.get('xpath', exc),
                                                 assertion.get('validate-xml', False),
                                                 assertion.get('ignore-whitespace', True),
                                                 assertion.get('use-tolerant-parser', False),
                                                 assertion.get('invert', False))
            elements.append(component)
            elements.append(etree.Element("hashTree"))

        return elements