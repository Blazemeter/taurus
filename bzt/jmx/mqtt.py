from bzt.jmx.base import JMX
from bzt import TaurusConfigError
from bzt.jmx.tools import ProtocolHandler
from lxml import etree


class MQTTProtocolHandler(ProtocolHandler):
    def get_sampler_pair(self, request):
        if request.method == 'connect':
            mqtt = self._get_connect_method(request)
        elif request.method == 'publish':
            mqtt = self._get_publish_method(request)
        elif request.method == 'subscribe':
            mqtt = self._get_subscribe_method(request)
        elif request.method == 'disconnect':
            mqtt = self._get_disconnect_method(request)
        else:
            raise TaurusConfigError(f'unsupported mqtt method: {request.method}')

        children = etree.Element("hashTree")
        return mqtt, children

    def _get_connect_method(self, request):
        pass

    def _get_publish_method(self, request):
        pass

    def _get_subscribe_method(self, request):
        pass

    def _get_disconnect_method(self, request):
        pass
