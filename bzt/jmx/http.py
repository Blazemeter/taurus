import json

from bzt.jmx.base import JMX
from bzt.jmx.tools import ProtocolHandler
from bzt.six import etree
from bzt.utils import get_host_ips, BetterDict


class HTTPProtocolHandler(ProtocolHandler):
    def _get_merged_ci_headers(self, scenario, req, header):
        def dic_lower(dic):
            return {str(k).lower(): str(dic[k]).lower() for k in dic}

        ci_scenario_headers = dic_lower(scenario.get_headers())
        ci_request_headers = dic_lower(req.headers)
        headers = BetterDict.from_dict(ci_scenario_headers)
        headers.merge(ci_request_headers)
        if header.lower() in headers:
            return headers[header]
        else:
            return None

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

        timeout = scenario.get("timeout", None)
        timeout = self.safe_time(timeout)
        elements = [JMX._get_http_defaults(default_address, timeout, retrieve_resources,
                                           concurrent_pool_size, content_encoding, resources_regex),
                    etree.Element("hashTree")]
        return elements

    def get_sampler_pair(self, scenario, request):
        timeout = self.safe_time(request.priority_option('timeout'))

        content_type = self._get_merged_ci_headers(scenario, request, 'content-type')
        if content_type == 'application/json' and isinstance(request.body, (dict, list)):
            body = json.dumps(request.body)
        else:
            body = request.body

        use_random_host_ip = request.priority_option('random-source-ip', default=False)
        host_ips = get_host_ips(filter_loopbacks=True) if use_random_host_ip else []

        files = request.upload_files
        body_file = request.config.get("body-file")
        has_file_for_body = not (body or files) and body_file

        # method can be put, post, or even variable
        if has_file_for_body and request.method != "GET":
            files = [{"path": body_file}]

        http = JMX._get_http_request(request.url, request.label, request.method, timeout, body,
                                     request.priority_option('keepalive', default=True),
                                     files, request.content_encoding,
                                     request.priority_option('follow-redirects', default=True),
                                     use_random_host_ip, host_ips)

        children = etree.Element("hashTree")

        if request.headers:
            children.append(JMX._get_header_mgr(request.headers))
            children.append(etree.Element("hashTree"))

        return http, children
