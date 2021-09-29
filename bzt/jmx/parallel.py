from lxml import etree

from bzt import TaurusConfigError
from bzt.jmx.base import JMX
from bzt.jmx.tools import RequestCompiler
from bzt.requests_model import HierarchicRequestParser, Request


class ParallelBlock(Request):
    NAME = "parallel"

    def __init__(self, do_clause, config):
        super(ParallelBlock, self).__init__(config)
        self.do_clause = do_clause

    def __repr__(self):
        do_clause = [repr(req) for req in self.do_clause]
        return "ParallelBlock(do=%s)" % do_clause


class ParallelRequestCompiler(RequestCompiler):
    def __init__(self, jmx_builder):
        super().__init__(jmx_builder)

    def compile_parallel_block(self, block):
        parallel_controller = self._get_parallel_controller()
        parallel_children = etree.Element("hashTree")
        for compiled in self.jmx_builder.compile_requests(block.do_clause):
            for element in compiled:
                parallel_children.append(element)

        return [parallel_controller, parallel_children]

    def _get_parallel_controller(self):
        controller = etree.Element("com.blazemeter.jmeter.controller.ParallelSampler",
                                   testname="Parallel Controller",
                                   testclass="com.blazemeter.jmeter.controller.ParallelSampler",
                                   guiclass="com.blazemeter.jmeter.controller.ParallelControllerGui")
        controller.append(JMX.int_prop("MAX_THREAD_NUMBER", 6))
        controller.append(JMX._bool_prop("PARENT_SAMPLE", False))
        controller.append(JMX._bool_prop("LIMIT_MAX_THREAD_NUMBER", False))
        return controller

    def visit_parallelblock(self, block):
        return self.compile_parallel_block(block)


class ProtocolRequestParser(HierarchicRequestParser):
    def _parse_request(self, req):
        if 'parallel' in req:
            do_clauses = req.get("do", TaurusConfigError("'do' clause is mandatory for 'parallel' blocks"))
            do_requests = self._parse_requests(do_clauses)
            return ParallelBlock(do_requests, req)

        else:
            return super()._parse_request(req)
