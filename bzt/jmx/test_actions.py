from lxml import etree

from bzt import TaurusConfigError
from bzt.jmx import JMX
from bzt.jmx.tools import ProtocolHandler
from bzt.six import iteritems
from bzt.utils import dehumanize_time


class TestActionsHandler(ProtocolHandler):
    def get_processor_elements(self, scenario, request):
        """
        Handles 'timeout', 'think-time', 'assertions'
        :param scenario:
        :param request:
        :return:
        """
        elements = []
        self.__add_think_time(elements, request)
        self.__add_timeout(elements, request)
        self._add_jsr_elements(elements, request)
        return elements

    def __add_think_time(self, children, request):
        think_time = request.priority_option('think-time')
        if think_time is not None:
            children.append(JMX._get_constant_timer(ProtocolHandler.safe_time(think_time)))
            children.append(etree.Element("hashTree"))

    def __add_timeout(self, children, request):
        timeout = ProtocolHandler.safe_time(request.priority_option('timeout'))
        if timeout is not None:
            children.append(JMX._get_dur_assertion(timeout))
            children.append(etree.Element("hashTree"))

