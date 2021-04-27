from bzt.jmx.base import JMX
from bzt import TaurusConfigError
from bzt.jmx.tools import ProtocolHandler
from bzt.utils import dehumanize_time, BetterDict
from lxml import etree


COUNT_CHECK_CONTENT = """
String resp_message = prev.getResponseMessage();
int target_count_min = %s
def match = (resp_message =~ /Received (\d+) of message./);
if (match.find()) {
    int received_messages = match[0][1].toInteger();
    if (target_count_min > received_messages) {
        prev.setResponseMessage("Number of received messages is too low: " + received_messages.toString());
        prev.setSuccessful(false);
    }
}
"""


class MQTTProtocolHandler(ProtocolHandler):
    def get_sampler_pair(self, request):
        if request.method == 'connect':
            mqtt = self._get_connect_sampler(request)
        elif request.method == 'publish':
            mqtt = self._get_publish_sampler(request)
        elif request.method == 'subscribe':
            mqtt = self._get_subscribe_sampler(request)

            min_count = request.config.get("min-count", 0)
            if min_count:
                groovy_script = self.engine.create_artifact("mqtt-subscribe", ".gsh")
                with open(groovy_script, 'w+') as gsh:
                    gsh.write(COUNT_CHECK_CONTENT % (min_count,))

                count_check = BetterDict()
                count_check['language'] = 'groovy'
                count_check['script-file'] = groovy_script
                count_check['execute'] = 'after'
                request.config.get("jsr223", default=[], force_set=True).append(count_check)

        elif request.method == 'disconnect':
            mqtt = self._get_disconnect_sampler(request)
        else:
            raise TaurusConfigError(f'Unsupported mqtt method: {request.method}')

        children = etree.Element("hashTree")
        return mqtt, children

    @staticmethod
    def _get_connect_sampler(request):
        mqtt = etree.Element("net.xmeter.samplers.ConnectSampler",
                             guiclass="net.xmeter.gui.ConnectSamplerUI",
                             testclass="net.xmeter.samplers.ConnectSampler",
                             testname=request.label)
        mqtt.append(JMX._string_prop("mqtt.server", request.config.get("addr", "127.0.0.1")))
        mqtt.append(JMX._string_prop("mqtt.port", "1883"))
        mqtt.append(JMX._string_prop("mqtt.version", "3.1"))
        mqtt.append(JMX._string_prop("mqtt.conn_timeout", "10"))
        mqtt.append(JMX._string_prop("mqtt.protocol", "TCP"))
        mqtt.append(JMX._string_prop("mqtt.ws_path", ""))
        mqtt.append(JMX._bool_prop("mqtt.dual_ssl_authentication", False))
        mqtt.append(JMX._string_prop("mqtt.clientcert_file_path", ""))
        mqtt.append(JMX._string_prop("mqtt.clientcert_password", ""))
        mqtt.append(JMX._string_prop("mqtt.client_id_prefix", "conn_"))
        mqtt.append(JMX._bool_prop("mqtt.client_id_suffix", True))
        mqtt.append(JMX._string_prop("mqtt.conn_keep_alive", "300"))
        mqtt.append(JMX._string_prop("mqtt.conn_attampt_max", "0"))
        mqtt.append(JMX._string_prop("mqtt.reconn_attampt_max", "0"))
        mqtt.append(JMX._string_prop("mqtt.conn_clean_session", "true"))

        return mqtt

    @staticmethod
    def _get_publish_sampler(request):
        mqtt = etree.Element("net.xmeter.samplers.PubSampler",
                             guiclass="net.xmeter.gui.PubSamplerUI",
                             testclass="net.xmeter.samplers.PubSampler",
                             testname=request.label)
        topic_missed = TaurusConfigError(f'Topic is required for request "{request.config}"')
        mqtt.append(JMX._string_prop("mqtt.topic_name", request.config.get("topic", topic_missed)))
        mqtt.append(JMX._string_prop("mqtt.qos_level", "0"))
        mqtt.append(JMX._bool_prop("mqtt.add_timestamp", True))
        mqtt.append(JMX._string_prop("mqtt.message_type", "String"))
        mqtt.append(JMX._string_prop("mqtt.message_type_fixed_length", "1024"))
        mqtt.append(JMX._string_prop("mqtt.message_to_sent", request.config.get("message", "")))
        mqtt.append(JMX._bool_prop("mqtt.retained_message", False))

        return mqtt

    @staticmethod
    def _get_subscribe_sampler(request):
        mqtt = etree.Element("net.xmeter.samplers.SubSampler",
                             guiclass="net.xmeter.gui.SubSamplerUI",
                             testclass="net.xmeter.samplers.SubSampler",
                             testname=request.label)
        topic_missed = TaurusConfigError(f'Topic is required for request "{request.config}"')
        time_interval = int(dehumanize_time(request.config.get("time", 1)))

        mqtt.append(JMX._string_prop("mqtt.topic_name", request.config.get("topic", topic_missed)))
        mqtt.append(JMX._string_prop("mqtt.qos_level", "0"))
        mqtt.append(JMX._bool_prop("mqtt.add_timestamp", False))
        mqtt.append(JMX._bool_prop("mqtt.debug_response", False))
        mqtt.append(JMX._string_prop("mqtt.sample_condition", "specified elapsed time (ms)"))
        mqtt.append(JMX._string_prop("mqtt.sample_condition_value", str(time_interval * 1000)))

        return mqtt

    @staticmethod
    def _get_disconnect_sampler(request):
        mqtt = etree.Element("net.xmeter.samplers.DisConnectSampler",
                             guiclass="net.xmeter.gui.DisConnectSamplerUI",
                             testclass="net.xmeter.samplers.DisConnectSampler",
                             testname=request.label)

        return mqtt
