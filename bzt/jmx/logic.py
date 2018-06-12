import re

from lxml import etree

from bzt import TaurusConfigError
from jmx import JMX
from jmx.tools import ProtocolHandler


class LogicProtocolHandler(ProtocolHandler):
    def get_toplevel_elements(self, scenario):
        return None

    def get_elements_for_request(self, scenario, request):
        if 'if' in request.config:
            condition = request.get("if")
            # TODO: apply some checks to `condition`?
            then_clause = request.get("then", TaurusConfigError("'then' clause is mandatory for 'if' blocks"))
            else_clause = request.get("else", [])
            return self.compile_if_block(condition, then_clause, else_clause, request)
        else:
            return None

    def compile_if_block(self, condition, then_clause, else_clause, request):
        elements = []

        if_controller = JMX._get_if_controller(condition)
        then_children = etree.Element("hashTree")
        for subelement in self.scenario_builder.compile_subrequests(then_clause):
            then_children.append(subelement)
        elements.extend([if_controller, then_children])

        if else_clause:
            inverted_condition = "!(" + condition + ")"
            else_controller = JMX._get_if_controller(inverted_condition)
            else_children = etree.Element("hashTree")
            for subelement in self.scenario_builder.compile_subrequests(else_clause):
                else_children.append(subelement)
            elements.extend([else_controller, else_children])

        return elements
