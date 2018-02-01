# import json
import time
import requests

from datetime import date, datetime
import logging


def timing(f):
    def wrap(*args):
        time1 = time.time()
        ret = f(*args)
        time2 = time.time()
        print('%s function took %0.3f ms' % (f.func_name, (time2-time1)*1000.0))
        return ret
    return wrap


def json_serial(obj):
    """JSON serializer for objects not serializable by default json code"""

    if isinstance(obj, (datetime, date)):
        return obj.isoformat()
    raise TypeError("Type %s not serializable" % type(obj))


def _attach_service(remote_service):
    base_service = remote_service["base_service"]

    response = requests.post(base_service + "attach",
                             json={"GroupUUID": remote_service["user_id"], "ServiceID": remote_service["service_id"]})
    # print(response.json())
    return response.json()


class Remote(object):
    def __getattribute__(self, name):
        attr = object.__getattribute__(self, name)
        if hasattr(attr, '__call__'):
            def newfunc(*args, **kwargs):
                start_time = 0
                if self.debug_time:
                    self.debug_time_ident += 1
                    start_time = time.time()
                result = attr(*args, **kwargs)
                if self.debug_time:
                    end_time = time.time()
                    self.log.info(('  ' * self.debug_time_ident) +
                                  '%s function took %0.3f ms' % (attr.__name__, (end_time - start_time) * 1000.0))
                return result
            return newfunc
        else:
            return attr

    def __init__(self, parent_logger, user_id):

        super(Remote, self).__init__()

        self.debug_time = False
        self.debug_time_ident = 0

        self.user_id = user_id
        self.log = parent_logger.getChild(self.__class__.__name__)

        self.base_service = "https://kip7rvk9ih.execute-api.us-east-1.amazonaws.com/dev/"

        # Disable boto logging and create remote clients
        logging.getLogger('boto3').setLevel(logging.CRITICAL)
        logging.getLogger('botocore').setLevel(logging.CRITICAL)

    def get_catalog(self):
        r = requests.post(self.base_service + "list")
        # print(r.status_code)
        return r.json()

    def detach_service(self, attach_id):

        response = requests.post(self.base_service + "detach",
                                 json={"GroupUUID": self.user_id, "AttachmentID": attach_id})
        # print(response.json())
        return response.json()

    def attach_services(self, service_ids):
        remote_service = []
        for service_id in service_ids:
            remote_service.append(
                {
                    "base_service": self.base_service,
                    "cluster_id": "",
                    "user_id": self.user_id,
                    "service_id": service_id
                }
            )

        attach_ids = map(_attach_service, remote_service)

        return attach_ids

    def wait_attach_ready(self, attach_id):
        while self.attach_status(attach_id)["service_state"] != "RUNNING":
            print("Waiting...")
            time.sleep(1)

    def attach_status(self, attach_id):
        status = self.list_attached(attach_id)
        if len(status) > 0:
            return status[0]
        else:
            return None

    def list_attached(self, attach_ids=None):

        response = requests.post(self.base_service + "status",
                                 json={"GroupUUID": self.user_id, "AttachmentID": attach_ids})

        attached = response.json()

        return attached

    def get_services(self, filter_services):

        attached = self.list_attached()
        filtered = []
        for attach in attached:
            service_id = attach["service_id"]
            if service_id in filter_services or len(filter_services) == 1 and filter_services[0] == "auto":
                filtered.append(attach)

        return filtered

    def pull_service(self, service_id, reserved=None):
        reserved = reserved or []

        # TODO: Migrate to lamda + database

        services = self.get_services([service_id])
        attach_id = None
        service_remote = None
        capabilities = []
        service_vnc = None
        if len(services) > 0:
            for attach in services:
                if  attach["service_state"] == "RUNNING":
                    attach_id = attach["attach_id"]
                    if "service_info" in attach and "selenium" in attach["service_info"]:

                        service_remote = attach["service_info"]["selenium"]["info"]["remote"]
                        service_vnc = attach["service_info"]["selenium"]["info"]["vnc"]
                        capabilities.append({"browser": attach["service_id"].split("-")[0]})

                        if attach_id not in reserved:
                            break
        return {"attach_id": attach_id , "remote": service_remote, "capabilities": capabilities, "vnc": service_vnc}
