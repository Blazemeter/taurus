import re

from lxml import etree

from bzt import TaurusConfigError
from jmx import JMX
from jmx.tools import ProtocolHandler
from utils import dehumanize_time


class LogicControllersHandler(ProtocolHandler):
    def get_toplevel_elements(self, scenario):
        return None

    def get_elements_for_request(self, scenario, request):
        if 'if' in request.config:
            condition = request.get("if")
            # TODO: apply some checks to `condition`?
            then_clause = request.get("then", TaurusConfigError("'then' clause is mandatory for 'if' blocks"))
            else_clause = request.get("else", [])
            return self.compile_if_block(condition, then_clause, else_clause, request)
        elif 'loop' in request.config:
            loops = request.get("loop")
            do_block = request.get("do", TaurusConfigError("'do' option is mandatory for 'loop' blocks"))
            return self.compile_loop_block(loops, do_block, request)
        elif 'while' in request.config:
            condition = request.get("while")
            do_block = request.get("do", TaurusConfigError("'do' option is mandatory for 'while' blocks"))
            return self.compile_while_block(condition, do_block, request)
        elif 'foreach' in request.config:
            iteration_str = request.get("foreach")
            match = re.match(r'(.+) in (.+)', iteration_str)
            if not match:
                msg = "'foreach' value should be in format '<elementName> in <collection>' but '%s' found"
                raise TaurusConfigError(msg % iteration_str)
            loop_var, input_var = match.groups()
            do_block = request.get("do", TaurusConfigError("'do' field is mandatory for 'foreach' blocks"))
            return self.compile_foreach_block(input_var, loop_var, do_block, request)
        elif 'transaction' in request.config:
            name = request.get('transaction')
            do_block = request.get('do', TaurusConfigError("'do' field is mandatory for transaction blocks"))
            include_timers = request.get('include-timers', False)
            force_parent_sample = request.priority_option('force-parent-sample', True)
            return self.compile_transaction_block(name, include_timers, force_parent_sample, do_block, request)
        elif 'include-scenario' in request.config:
            name = request.get('include-scenario')
            return self.compile_include_scenario_block(name)
        else:
            return None

    def compile_if_block(self, condition, then_clause, else_clause, request):
        elements = []
        label = request.get('label', 'If %r' % condition)
        else_label = request.get('else-label', 'If not %r' % condition)

        if_controller = JMX._get_if_controller(condition, label)
        then_children = etree.Element("hashTree")
        for subelement in self.scenario_builder.compile_subrequests(then_clause):
            then_children.append(subelement)
        elements.extend([if_controller, then_children])

        if else_clause:
            inverted_condition = "!(" + condition + ")"
            else_controller = JMX._get_if_controller(inverted_condition, else_label)
            else_children = etree.Element("hashTree")
            for subelement in self.scenario_builder.compile_subrequests(else_clause):
                else_children.append(subelement)
            elements.extend([else_controller, else_children])

        return elements

    def compile_loop_block(self, loops, do_block, request):
        label = request.get('label', None)

        loop_controller = JMX._get_loop_controller(loops, label)
        children = etree.Element("hashTree")
        for subelement in self.scenario_builder.compile_subrequests(do_block):
            children.append(subelement)

        return [loop_controller, children]

    def compile_while_block(self, condition, do_block, request):
        label = request.get('label', None)

        controller = JMX._get_while_controller(condition, label)
        children = etree.Element("hashTree")
        for subelement in self.scenario_builder.compile_subrequests(do_block):
            children.append(subelement)
        return [controller, children]

    def compile_foreach_block(self, input_var, loop_var, do_block, request):
        label = request.get('label', None)

        controller = JMX._get_foreach_controller(input_var, loop_var, label)
        children = etree.Element("hashTree")
        for subelement in self.scenario_builder.compile_subrequests(do_block):
            children.append(subelement)

        return [controller, children]

    def compile_transaction_block(self, name, include_timers, force_parent_sample, do_block, request):
        controller = JMX._get_transaction_controller(name, force_parent_sample, include_timers)

        children = etree.Element("hashTree")
        for subelement in self.scenario_builder.compile_subrequests(do_block):
            children.append(subelement)

        return [controller, children]

    def compile_include_scenario_block(self, scenario_name):
        controller = JMX._get_simple_controller(scenario_name)
        children = etree.Element("hashTree")

        scenario = self.scenario_builder.executor.get_scenario(name=scenario_name)
        requests = scenario.get('requests', [])

        for subelement in self.scenario_builder.compile_subrequests(requests):
            children.append(subelement)

        return [controller, children]
