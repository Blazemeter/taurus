import logging
from lxml import etree

from bzt.jmx.base import JMX
from bzt.jmx.tools import ProtocolHandler
from bzt.utils import get_host_ips, convert_body_to_string

LOG = logging.getLogger("")


class HTTPProtocolHandler(ProtocolHandler):
    def get_toplevel_elements(self, scenario):
        return self._gen_managers(scenario) + self._gen_defaults(scenario)

    def _gen_managers(self, scenario):
        elements = []
        if scenario.get("store-cache", True):
            elements.append(JMX._get_cache_mgr())
            elements.append(etree.Element("hashTree"))
        if scenario.get("store-cookie", True):
            elements.append(JMX._get_cookie_mgr(scenario))
            elements.append(etree.Element("hashTree"))
        if scenario.get("use-dns-cache-mgr", True):
            elements.append(JMX.get_dns_cache_mgr())
            elements.append(etree.Element("hashTree"))
            self.system_props.merge({"system-properties": {"sun.net.inetaddr.ttl": 0}})
        return elements

    def _gen_defaults(self, scenario):
        default_address = scenario.get("default-address", None)
        retrieve_resources = scenario.get("retrieve-resources", True)
        resources_regex = scenario.get("retrieve-resources-regex", None)
        concurrent_pool_size = scenario.get("concurrent-pool-size", 4)

        content_encoding = scenario.get("content-encoding", None)

        timeout = scenario.get("timeout", "30s")
        timeout = self.safe_time(timeout)
        elements = [JMX._get_http_defaults(default_address, timeout, retrieve_resources,
                                           concurrent_pool_size, content_encoding, resources_regex),
                    etree.Element("hashTree")]
        return elements

    def get_sampler_pair(self, request):
        convert_body_to_string(request)
        use_random_host_ip = request.priority_option('random-source-ip', default=False)
        host_ips = get_host_ips(filter_loopbacks=True) if use_random_host_ip else []

        files = request.upload_files
        body_file = request.config.get("body-file")
        has_file_for_body = not (request.body or files) and body_file

        # method can be put, post, or even variable
        if has_file_for_body and request.method != "GET":
            files = [{"path": body_file}]

        timeout = self.safe_time(request.config.get("timeout"))

        http = JMX._get_http_request(request.url, request.label, request.method, timeout, request.body,
                                     request.priority_option('keepalive', default=True),
                                     files, request.content_encoding,
                                     request.priority_option('follow-redirects', default=True),
                                     use_random_host_ip, host_ips)

        children = etree.Element("hashTree")

        if request.headers:
            children.append(JMX._get_header_mgr(request.headers))
            children.append(etree.Element("hashTree"))

        return http, children
