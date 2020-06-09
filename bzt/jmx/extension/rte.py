import logging
import re

from bzt.jmx.base import JMX
from bzt.jmx.tools import ProtocolHandler
from lxml import etree

RTE_CLASS_PATH = "com.blazemeter.jmeter.rte.sampler."

RTE_CONNECTION_CONFIG_ = "RTEConnectionConfig."

RTE_SAMPLER_ = "RTESampler."

LOG = logging.getLogger("")


class RTEProtocolHandler(ProtocolHandler):
    def get_toplevel_elements(self, scenario):
        if hasattr(scenario, 'data') and "protocol" in scenario.data and scenario.data['protocol'] == "rte":
            return self._gen_server_config(scenario)
        return []

    def _gen_server_config(self, scenario):
        cfg = etree.Element("ConfigTestElement", guiclass="com.blazemeter.jmeter.rte.sampler.gui.RTEConfigGui",
                            testclass="ConfigTestElement", testname="Server Config")
        data = scenario.data
        server = data.get("rte-server").split(":") if "rte-server" in data else None
        if server:
            cfg.append(JMX._string_prop("%sserver" % RTE_CONNECTION_CONFIG_, server[0]))
            cfg.append(JMX._string_prop("%sport" % RTE_CONNECTION_CONFIG_, server[1])) if len(server) > 1 else None

        cfg.append(
            JMX._string_prop("%sprotocol" % RTE_CONNECTION_CONFIG_, data['rte-protocol'])) if 'rte-protocol' in data \
            else None

        if 'rte-config' in data:
            config = data.get('rte-config')
            cfg.append(
                JMX._string_prop("%ssslType" % RTE_CONNECTION_CONFIG_, config['ssl'])) if 'ssl' in config else None
            cfg.append(JMX._string_prop("%sterminalType" % RTE_CONNECTION_CONFIG_, config['terminal-type'])) if \
                'terminal-type' in config else None
            connection_timeout = self.safe_time(config['connection-timeout'])
            cfg.append(JMX._string_prop("%sconnectTimeout" % RTE_CONNECTION_CONFIG_, connection_timeout)) if \
                'connection-timeout' in config else None

        return [cfg, etree.Element("hashTree")]

    def get_sampler_pair(self, request):
        action = etree.Element("%sRTESampler" % RTE_CLASS_PATH,
                               guiclass=("%sgui.RTESamplerGui" % RTE_CLASS_PATH),
                               testclass=RTE_CLASS_PATH, testname=request.label)

        action.append(JMX._string_prop("RTESampler.action", request.config.get('rte-action')))
        if "wait-for" in request.config:
            self._gen_wait(action, request.config.get("wait-for"))

        if 'fields' in request.config:
            all_input = JMX._collection_prop("Inputs.inputs")
            for _input in request.config["fields"]:
                all_input.append(self._gen_input_prop(_input))
            elem_props = JMX._element_prop("Inputs.inputs", "%sInputs" % RTE_CLASS_PATH)
            elem_props.append(all_input)
            action.append(elem_props)

        if 'attention-key' in request.config:
            action.append(JMX._string_prop("RTESampler.attentionKey", request.config.get('attention-key')))

        children = etree.Element("hashTree")
        if 'extract-cursor-position' in request.config and request.config.get('extract-cursor-position'):
            elem = request.config.get('extract-cursor-position')

            ext = self._gen_rte_extractor(elem['varname'], "CURSOR_POSITION")
            children.append(ext)
            children.append(etree.Element("hashTree"))  # Is this an RTE extension bug? doesn't seem necesary,
            # but won't work without it.

        if 'extract-field-position' in request.config and request.config.get('extract-field-position'):
            elem = request.config.get('extract-field-position')
            position = elem['base-position'].strip("()")
            row, column = position.split(",")
            offset = elem['field-offset']
            ext = self._gen_rte_extractor(elem['varname'], "NEXT_FIELD_POSITION", row, column, offset)
            children.append(ext)
            children.append(etree.Element("hashTree"))

        return action, children

    @staticmethod
    def _gen_rte_extractor(varname, type, row="", column="", offset=""):
        extractor = etree.Element("com.blazemeter.jmeter.rte.extractor.RTEExtractor",
                                  guiclass="com.blazemeter.jmeter.rte.extractor.RTEExtractorGui",
                                  testclass="com.blazemeter.jmeter.rte.extractor.RTEExtractor",
                                  testname="Cursor extractor")
        extractor.append(JMX._string_prop("RTEExtractor.variablePrefix", varname))
        extractor.append(JMX._string_prop("RTEExtractor.positionType", type))
        extractor.append(JMX._string_prop("RTEExtractor.row", row))
        extractor.append(JMX._string_prop("RTEExtractor.offset", offset))
        extractor.append(JMX._string_prop("RTEExtractor.column", column))
        return extractor

    @staticmethod
    def _gen_input_prop(_input):
        row, col, label, text, tabs = RTEProtocolHandler._handle_input_notaiton(_input)
        _prop_base = "com.blazemeter.jmeter.rte.sampler."
        if row and col:
            prop = JMX._element_prop("elementProp", "%sCoordInputRowGUI" % _prop_base)
            prop.append(JMX._string_prop("CoordInputRowGUI.row", row))
            prop.append(JMX._string_prop("CoordInputRowGUI.column", col))
        elif label:
            prop = JMX._element_prop("elementProp", "%sLabelInputRowGUI" % _prop_base)
            prop.append(JMX._string_prop("LabelInputRowGUI.label", label))
        elif tabs is not None:
            prop = JMX._element_prop("elementProp", "%sTabulatorInputRowGui" % _prop_base)
            prop.append(JMX._string_prop("TabulatorInputOffsetGUI.column", tabs))
        prop.append(JMX._string_prop("Input.input", text))
        return prop

    @staticmethod
    def _handle_input_notaiton(_input):
        row = _input['row'] if 'row' in _input else None
        col = _input['column'] if 'column' in _input else None
        label = _input["label"] if 'label' in _input else None
        text = _input["text"] if 'text' in _input else None
        tabs = _input["tabs"] if 'tabs' in _input else None

        # Handle shorthand declarations
        for key, value in _input.items():
            if re.match(r'\(\d+,\d+\)', key):
                row, col = re.findall(r'\d+', key)
                text = _input[key]

            elif re.match(r'\(.+?\)', key):
                label = re.findall("\((.+?)\)", key)[0]
                text = _input[key]
        return row, col, label, text, tabs

    def _gen_wait(self, action, wait_for):
        action.append(JMX._bool_prop("%swaitSync" % RTE_SAMPLER_, 'sync' in wait_for))
        if 'sync' in wait_for:
            sync_timeout = self.safe_time(wait_for['sync'])
            action.append(JMX._string_prop("%swaitSyncTimeout" % RTE_SAMPLER_, sync_timeout))

        action.append(JMX._bool_prop("%swaitCursor" % RTE_SAMPLER_, 'cursor' in wait_for))
        if 'cursor' in wait_for:
            cursor = wait_for['cursor']
            if type(cursor) is str:
                row, column = cursor.split(',')
            else:
                row, column = cursor['position'].split(',')
            action.append(JMX._string_prop("%swaitCursorRow" % RTE_SAMPLER_, row))
            action.append(JMX._string_prop("%swaitCursorColumn" % RTE_SAMPLER_, column))
            if 'timeout' in cursor:
                cursor_timeout = self.safe_time(cursor['timeout'])
                action.append(JMX._string_prop("%swaitCursorTimeout" % RTE_SAMPLER_, cursor_timeout))

        action.append(JMX._bool_prop("%swaitSilent" % RTE_SAMPLER_, 'silent' in wait_for))
        if 'silent' in wait_for:
            silent = wait_for['silent']
            if type(silent) is str:
                stable_time = self.safe_time(silent)
            else:
                stable_time = self.safe_time(silent['stable-time'])

            action.append(JMX._string_prop("%swaitSilentTime" % RTE_SAMPLER_, stable_time))
            if 'timeout' in silent:
                silent_timeout = self.safe_time(silent['timeout'])
                action.append(JMX._string_prop("%swaitSilentTimeout" % RTE_SAMPLER_, silent_timeout))

        action.append(JMX._bool_prop("%swaitText" % RTE_SAMPLER_, 'text' in wait_for))
        if 'text' in wait_for:
            text = wait_for['text']
            if type(text) is str:
                regex = text
            else:
                regex = text['regex']
            action.append(JMX._string_prop("%swaitTextRegex" % RTE_SAMPLER_, regex))
            timeout = self.safe_time(text['timeout'])
            action.append(JMX._string_prop("%swaitTextTimeout" % RTE_SAMPLER_, timeout)) if \
                'timeout' in text else None
            if 'search-area' in text:
                top, left, bot, right = re.findall(r'\d+', text['search-area'])
                action.append(JMX._string_prop("%swaitTextAreaTop" % RTE_SAMPLER_, top))
                action.append(JMX._string_prop("%swaitTextAreaLeft" % RTE_SAMPLER_, left))
                action.append(JMX._string_prop("%swaitTextAreaBottom" % RTE_SAMPLER_, bot))
                action.append(JMX._string_prop("%swaitTextAreaRight" % RTE_SAMPLER_, right))
