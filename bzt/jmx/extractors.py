from distutils.version import LooseVersion

from lxml import etree

from bzt import TaurusConfigError
from bzt.jmx import JMX
from bzt.jmx.tools import ProtocolHandler
from bzt.six import iteritems
from bzt.utils import ensure_is_dict


class ExtractorsHandler(ProtocolHandler):
    def get_processor_elements(self, scenario, request):
        elements = []
        self.__add_boundary_ext(elements, request)
        self.__add_regexp_ext(elements, request)
        self.__add_json_ext(elements, request)
        self.__add_jquery_ext(elements, request)
        self.__add_xpath_ext(elements, request)
        return elements

    def __add_boundary_ext(self, children, req):
        extractors = req.config.get("extract-boundary")
        for varname, cfg in iteritems(extractors):
            subj = cfg.get('subject', 'body')
            left = cfg.get('left', TaurusConfigError("Left boundary is missing for boundary extractor %s" % varname))
            right = cfg.get('right', TaurusConfigError("Right boundary is missing for boundary extractor %s" % varname))
            match_no = cfg.get('match-no', 1)
            defvalue = cfg.get('default', 'NOT_FOUND')
            extractor = JMX._get_boundary_extractor(varname, subj, left, right, match_no, defvalue)
            children.append(extractor)
            children.append(etree.Element("hashTree"))

    def __add_regexp_ext(self, children, req):
        extractors = req.config.get("extract-regexp")
        for varname in extractors:
            cfg = ensure_is_dict(extractors, varname, "regexp")
            extractor = JMX._get_extractor(varname, cfg.get('subject', 'body'), cfg['regexp'], cfg.get('template', 1),
                                           cfg.get('match-no', 1), cfg.get('default', 'NOT_FOUND'))
            children.append(extractor)
            children.append(etree.Element("hashTree"))

    def __add_json_ext(self, children, req):
        jextractors = req.config.get("extract-jsonpath")
        for varname in jextractors:
            cfg = ensure_is_dict(jextractors, varname, "jsonpath")
            if LooseVersion(str(self.scenario_builder.executor.settings.get("version"))) < LooseVersion("3.0"):
                extractor = JMX._get_json_extractor(varname,
                                                    cfg["jsonpath"],
                                                    cfg.get("default", "NOT_FOUND"),
                                                    cfg.get("from-variable", None))
            else:
                extractor = JMX._get_internal_json_extractor(varname,
                                                             cfg["jsonpath"],
                                                             cfg.get("default", "NOT_FOUND"),
                                                             cfg.get("scope", None),
                                                             cfg.get("from-variable", None),
                                                             cfg.get("match-no", "0"),
                                                             cfg.get("concat", False))

            children.append(extractor)
            children.append(etree.Element("hashTree"))

    def __add_jquery_ext(self, children, req):
        css_jquery_extors = req.config.get("extract-css-jquery")
        for varname in css_jquery_extors:
            cfg = ensure_is_dict(css_jquery_extors, varname, "expression")
            extractor = JMX._get_jquerycss_extractor(varname,
                                                     cfg['expression'],
                                                     cfg.get('attribute', ""),
                                                     cfg.get('match-no', 0),
                                                     cfg.get('default', 'NOT_FOUND'))
            children.append(extractor)
            children.append(etree.Element("hashTree"))

    def __add_xpath_ext(self, children, req):
        xpath_extractors = req.config.get("extract-xpath")
        for varname in xpath_extractors:
            cfg = ensure_is_dict(xpath_extractors, varname, "xpath")
            children.append(JMX._get_xpath_extractor(varname,
                                                     cfg['xpath'],
                                                     cfg.get('default', 'NOT_FOUND'),
                                                     cfg.get('validate-xml', False),
                                                     cfg.get('ignore-whitespace', True),
                                                     cfg.get("match-no", "-1"),
                                                     cfg.get('use-namespaces', False),
                                                     cfg.get('use-tolerant-parser', False)))
            children.append(etree.Element("hashTree"))
