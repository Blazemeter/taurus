import re

from bzt.jmx.base import JMX
from bzt.jmx.tools import ProtocolHandler
from lxml import etree


class RTEProtocolHandler(ProtocolHandler):
    def get_toplevel_elements(self, scenario):
        if hasattr(scenario, 'data') and "protocol" in scenario.data and scenario.data['protocol'] == "rte":
            return self._gen_server_config(scenario.data)
        return []

    def _gen_server_config(self, config_data):
        config = etree.Element("ConfigTestElement",
                            guiclass="com.blazemeter.jmeter.rte.sampler.gui.RTEConfigGui",
                            testclass="ConfigTestElement",
                            testname="Server Config")

        if "rte-server" in config_data:
            server = config_data.get("rte-server").split(":")
            config.append(JMX._string_prop("RTEConnectionConfig.server", server[0]))
            if len(server) > 1:
                config.append(JMX._string_prop("RTEConnectionConfig.port", server[1]))

        if 'rte-protocol' in config_data:
            config.append(JMX._string_prop("RTEConnectionConfig.protocol", config_data['rte-protocol']))

        if 'rte-config' in config_data:
            rte_config = config_data.get('rte-config')
            if 'ssl' in rte_config:
                config.append(JMX._string_prop("RTEConnectionConfig.sslType", rte_config['ssl']))
            if 'terminal-type' in rte_config:
                config.append(JMX._string_prop("RTEConnectionConfig.terminalType", rte_config['terminal-type']))
            if 'connection-timeout' in rte_config:
                connection_timeout = self.safe_time(rte_config['connection-timeout'])
                config.append(JMX._string_prop("RTEConnectionConfig.connectTimeout", connection_timeout))

        return [config, etree.Element("hashTree")]

    def get_sampler_pair(self, request):
        action = etree.Element("com.blazemeter.jmeter.rte.sampler.RTESampler",
                               guiclass="com.blazemeter.jmeter.rte.sampler.gui.RTESamplerGui",
                               testclass="com.blazemeter.jmeter.rte.sampler.",
                               testname=request.label)

        action.append(JMX._string_prop("RTESampler.action", request.config.get('rte-action')))
        if "wait-for" in request.config:
            self._gen_wait(action, request.config.get("wait-for"))

        if 'fields' in request.config:
            inputs = JMX._collection_prop("Inputs.inputs")
            for _input in request.config["fields"]:
                inputs.append(self._gen_input_prop(_input))
            elem_props = JMX._element_prop("Inputs.inputs", "com.blazemeter.jmeter.rte.sampler.Inputs")
            elem_props.append(inputs)
            action.append(elem_props)

        if 'attention-key' in request.config:
            action.append(JMX._string_prop("RTESampler.attentionKey", request.config.get('attention-key')))

        children = etree.Element("hashTree")
        if 'extract-cursor-position' in request.config and request.config.get('extract-cursor-position'):
            element = request.config.get('extract-cursor-position')

            extractor = self._gen_rte_extractor(element['varname'], "CURSOR_POSITION")
            children.append(extractor)
            children.append(etree.Element("hashTree"))  # Is this an RTE extension bug? doesn't seem necesary,
            # but won't work without it.

        if 'extract-field-position' in request.config and request.config.get('extract-field-position'):
            element = request.config.get('extract-field-position')
            position = element['base-position'].strip("()")
            row, column = position.split(",")
            offset = element['field-offset']
            extractor = self._gen_rte_extractor(element['varname'], "NEXT_FIELD_POSITION", row, column, offset)
            children.append(extractor)
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
        row, col, label, text, tabs = RTEProtocolHandler._handle_input_notation(_input)
        if row and col:
            prop = JMX._element_prop("elementProp", "com.blazemeter.jmeter.rte.sampler.CoordInputRowGUI")
            prop.append(JMX._string_prop("CoordInputRowGUI.row", row))
            prop.append(JMX._string_prop("CoordInputRowGUI.column", col))
        elif label:
            prop = JMX._element_prop("elementProp", "com.blazemeter.jmeter.rte.sampler.LabelInputRowGUI")
            prop.append(JMX._string_prop("LabelInputRowGUI.label", label))
        elif tabs is not None:
            prop = JMX._element_prop("elementProp", "com.blazemeter.jmeter.rte.sampler.TabulatorInputRowGui")
            prop.append(JMX._string_prop("TabulatorInputOffsetGUI.column", tabs))
        prop.append(JMX._string_prop("Input.input", text))
        return prop

    @staticmethod
    def _handle_input_notation(_input):
        row = _input.get('row', None)
        col = _input.get('column', None)
        label = _input.get('label', None)
        text = _input.get('text', None)
        tabs = _input.get('tabs', None)

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
        action.append(JMX._bool_prop("RTESampler.waitSync", 'sync' in wait_for))
        if 'sync' in wait_for:
            sync_timeout = self.safe_time(wait_for['sync'])
            action.append(JMX._string_prop("RTESampler.waitSyncTimeout", sync_timeout))

        action.append(JMX._bool_prop("RTESampler.waitCursor", 'cursor' in wait_for))
        if 'cursor' in wait_for:
            cursor = wait_for['cursor']
            if type(cursor) is str:
                row, column = cursor.split(',')
            else:
                row, column = cursor['position'].split(',')
            action.append(JMX._string_prop("RTESampler.waitCursorRow", row))
            action.append(JMX._string_prop("RTESampler.waitCursorColumn", column))
            if 'timeout' in cursor:
                cursor_timeout = self.safe_time(cursor['timeout'])
                action.append(JMX._string_prop("RTESampler.waitCursorTimeout", cursor_timeout))

        action.append(JMX._bool_prop("RTESampler.waitSilent", 'silent' in wait_for))
        if 'silent' in wait_for:
            silent = wait_for['silent']
            if type(silent) is str:
                stable_time = self.safe_time(silent)
            else:
                stable_time = self.safe_time(silent['stable-time'])

            action.append(JMX._string_prop("RTESampler.waitSilentTime", stable_time))
            if 'timeout' in silent:
                silent_timeout = self.safe_time(silent['timeout'])
                action.append(JMX._string_prop("RTESampler.waitSilentTimeout", silent_timeout))

        action.append(JMX._bool_prop("RTESampler.waitText", 'text' in wait_for))
        if 'text' in wait_for:
            text = wait_for['text']
            if type(text) is str:
                regex = text
            else:
                regex = text['regex']
            action.append(JMX._string_prop("RTESampler.waitTextRegex", regex))

            if 'timeout' in text:
                timeout = self.safe_time(text['timeout'])
                action.append(JMX._string_prop("RTESampler.waitTextTimeout", timeout))

            if 'search-area' in text:
                top, left, bot, right = re.findall(r'\d+', text['search-area'])
                action.append(JMX._string_prop("RTESampler.waitTextAreaTop", top))
                action.append(JMX._string_prop("RTESampler.waitTextAreaLeft", left))
                action.append(JMX._string_prop("RTESampler.waitTextAreaBottom", bot))
                action.append(JMX._string_prop("RTESampler.waitTextAreaRight", right))
