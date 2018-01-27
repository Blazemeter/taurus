import boto3
import json
import time
from selenium import webdriver

from datetime import date, datetime
import logging

def json_serial(obj):
    """JSON serializer for objects not serializable by default json code"""

    if isinstance(obj, (datetime, date)):
        return obj.isoformat()
    raise TypeError("Type %s not serializable" % type(obj))


class Remote(object):

    def __init__(self, parent_logger, user_id):

        self.user_id = user_id
        self.cluster_id = 'test-cluster'
        self.log = parent_logger.getChild(self.__class__.__name__)

        # Disable boto logging and create remote clients
        logging.getLogger('boto3').setLevel(logging.CRITICAL)
        logging.getLogger('botocore').setLevel(logging.CRITICAL)
        self.client_ecs = boto3.client('ecs')
        self.client_ec2 = boto3.client('ec2')

    def get_attach_service_state(self, service_id, service_ip, service_port):
        service_state = "PENDING"
        browser_name = service_id.split("-")[0]
        remote_url = "http://" + service_ip + ":" + service_port + "/wd/hub"
        service_caps = self.get_attach_service_capabilities(remote_url, browser_name)
        info = {}
        if service_caps:
            service_state = "RUNNING"
            info["platform"] = service_caps["platform"]
            info["browserName"] = service_caps["browserName"]
            info["version"] = service_caps["version"]
            info["remote"] = remote_url
        service_info = {
            "service_state": service_state,
            "info": info
        }

        return service_info

    def get_attach_service_capabilities(self, remote_url, browser_name):

        driver = webdriver.Remote(
            command_executor=remote_url,
            desired_capabilities={
                'browserName': browser_name,
            }
        )
        capabilities = driver.capabilities
        driver.quit()

        #print(json.dumps(capabilities, indent=4, sort_keys=False, default=json_serial))
        return capabilities

    def detach_service(self, attach_id, reason=""):
        response = self.client_ecs.stop_task(
            cluster=self.cluster_id,
            task=attach_id,
            reason=reason
        )
        #print(json.dumps(response, indent=4, sort_keys=False, default=json_serial))

    def attach_service(self, service_id):
        # http://boto3.readthedocs.io/en/latest/reference/services/ecs.html#ECS.Client.run_task
        response = self.client_ecs.run_task(
            cluster=self.cluster_id,
            launchType='FARGATE',
            taskDefinition=service_id,
            startedBy=self.user_id,
            group='service',
            count=1,
            platformVersion='LATEST',
            networkConfiguration={
                'awsvpcConfiguration': {
                    'subnets': [
                        'subnet-037a175e'
                    ],
                    'securityGroups': [
                        'sg-65a9b111'
                    ],
                    'assignPublicIp': 'ENABLED'
                }
            }
        )
        #print(response["failures"])

        attach_id = response["tasks"][0]["taskArn"].split("task/")[1]
        #print(attach_id)
        #print(json.dumps(response, indent=4, sort_keys=False, default=json_serial))

        return attach_id

    def list_containers(self):
        response = self.client_ecs.list_container_instances(
            cluster=self.cluster_id
        )
        print(json.dumps(response, indent=4, sort_keys=False, default=json_serial))

    def get_net_interface_from_eni(self, eni):
        response = self.client_ec2.describe_network_interfaces(
            NetworkInterfaceIds=[
                eni,
            ]
        )
        return response["NetworkInterfaces"][0]

    def list_net_interfaces(self):
        # http://boto3.readthedocs.io/en/latest/reference/services/ec2.html?highlight=network%20interfaces
        response = self.client_ec2.describe_network_interfaces(
            NetworkInterfaceIds=[
                'eni-333f4cef', 'eni-5a324186'
            ]
        )
        for net_int in response["NetworkInterfaces"]:
            print(net_int["NetworkInterfaceId"])
            print(net_int["PrivateIpAddress"])
            print(net_int["Association"]["PublicIp"])

        # print(json.dumps(response, indent=4, sort_keys=False, default=json_serial))

    def list_attributes(self):
        response = self.client_ecs.list_attributes(
            cluster=self.cluster_id,
            targetType='container',
        )
        #print(json.dumps(response, indent=4, sort_keys=False, default=json_serial))

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
        # http://boto3.readthedocs.io/en/latest/reference/services/ecs.html#ECS.Client.list_tasks

        attached = []
        filtered_count = 0
        response = self.client_ecs.list_tasks(
            cluster=self.cluster_id,
            startedBy=self.user_id
        )
        if len(response["taskArns"]) > 0:
            response = self.client_ecs.describe_tasks(
                cluster=self.cluster_id,
                tasks=response["taskArns"]
            )

            for task_desc in response["tasks"]:
                service_id = task_desc["taskDefinitionArn"].split("task-definition/")[1].split(":")[0]
                task_id = task_desc["taskArn"].split("task/")[1]

                if attach_ids and task_id in attach_ids or not attach_ids:
                    if attach_ids and task_id in attach_ids:
                        filtered_count += 1
                    last_status = task_desc["lastStatus"]
                    eni = None
                    for attach in task_desc["attachments"]:
                        if attach["type"] == "ElasticNetworkInterface":
                            for detail in attach["details"]:
                                if detail["name"] == "networkInterfaceId":
                                    eni = detail["value"]
                                    break

                    net_inf = None
                    private_ip = ""
                    public_ip = ""

                    if eni and last_status == "RUNNING":
                        net_inf = self.get_net_interface_from_eni(eni)
                    if net_inf:
                        private_ip = net_inf["PrivateIpAddress"]
                        if "Association" in net_inf:
                            public_ip = net_inf["Association"]["PublicIp"]

                    service_status = "PENDING"
                    service_info = {}
                    if last_status == "RUNNING":
                        service_port = "4444"
                        service_selenium_info = self.get_attach_service_state(service_id, public_ip, service_port)
                        service_status = service_selenium_info["service_state"]
                        service_info = {
                            "selenium": service_selenium_info,
                            "vnc": {
                                "info": {
                                    "remote": public_ip + ":5900",
                                    "pass": "secret"
                                },
                                "service_state": "RUNNING"
                            }
                        }

                    attach = {
                        "service_id": service_id,
                        "attach_id": task_id,
                        "machine_state": last_status,
                        "service_state": service_status,
                        "private_ip": private_ip,
                        "public_ip": public_ip,
                        "service_info": service_info
                    }
                    attached.append(attach)

                    #print("service_id:" + service_id)
                    #print("attach_id:" + task_id)
                    #print("machine_state:" + last_status)
                    #print("service_state:" + service_status)
                    #print("public_ip:" + public_ip)
                    #print("service_info:\n" + json.dumps(service_info, indent=4, sort_keys=False, default=json_serial))
                    #print("-------------------")

                    if attach_ids and filtered_count >= len(attach_ids):
                        break
                    # print(json.dumps(task_desc, indent=4, sort_keys=False, default=json_serial))
                    # print(json.dumps(net_inf, indent=4, sort_keys=False, default=json_serial))

        else:
            pass
            #print(json.dumps(response, indent=4, sort_keys=False, default=json_serial))

        return attached
