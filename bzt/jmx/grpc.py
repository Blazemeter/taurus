import json
from urllib import parse

from bzt.jmx.base import JMX
from bzt.jmx.tools import ProtocolHandler
from lxml import etree

numeric_types = (int, float, complex)


class GRPCProtocolHandler(ProtocolHandler):
    def get_sampler_pair(self, request):
        if isinstance(request.body, dict):
            request.body = json.dumps(request.body)

        if isinstance(request.metadata, dict):
            request.metadata = json.dumps(request.metadata)

        parsed_url = parse.urlparse(request.url)
        timeout = self.safe_time(request.timeout)

        full_method = parsed_url.path
        if full_method[0] == '/':
            full_method = full_method[1:]

        grpc = etree.Element("vn.zalopay.benchmark.GRPCSampler",
                             guiclass="vn.zalopay.benchmark.GRPCSamplerGui",
                             testclass="vn.zalopay.benchmark.GRPCSampler",
                             testname=request.label)

        grpc.append(JMX._string_prop("GRPCSampler.protoFolder", request.protoFolder))
        grpc.append(JMX._string_prop("GRPCSampler.libFolder", request.libFolder))
        grpc.append(JMX._string_prop("GRPCSampler.metadata", request.metadata))
        grpc.append(JMX._bool_prop("GRPCSampler.tls", parsed_url.scheme == "https"))
        grpc.append(JMX._bool_prop("GRPCSampler.tlsDisableVerification", request.tlsDisableVerification))
        grpc.append(JMX._string_prop("GRPCSampler.host", parsed_url.hostname))
        grpc.append(JMX._string_prop("GRPCSampler.port", parsed_url.port))
        grpc.append(JMX._string_prop("GRPCSampler.fullMethod", full_method))
        grpc.append(JMX._string_prop("GRPCSampler.deadline", timeout))
        grpc.append(JMX._string_prop("GRPCSampler.channelAwaitTermination", timeout))
        grpc.append(JMX._string_prop("GRPCSampler.maxInboundMessageSize", request.maxInboundMessageSize))
        grpc.append(JMX._string_prop("GRPCSampler.maxInboundMetadataSize", request.maxInboundMetadataSize))
        grpc.append(JMX._string_prop("GRPCSampler.requestJson", request.body))

        children = etree.Element("hashTree")
        return grpc, children
