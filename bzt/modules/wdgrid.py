import copy
import multiprocessing
import time

from bzt import TaurusConfigError, NormalShutdown
from bzt.bza import User, WDGridImages
from bzt.engine import ScenarioExecutor
from bzt.modules import vncviewer
from bzt.modules.blazemeter import CloudProvisioning
from bzt.modules.provisioning import Local
from bzt.six import text_type, urlparse
from bzt.utils import to_json


def _start_vnc(params):
    (params[0], 'secret', params[1], params[2])


class WDGridProvisioning(Local):
    GRID = "grid"

    def __init__(self):
        super(WDGridProvisioning, self).__init__()
        self.user = User()
        self._involved_engines = []
        self._vncs_pool = None

    def prepare(self):
        CloudProvisioning.merge_with_blazemeter_config(self)
        CloudProvisioning.configure_client(self)
        self.__dump_status_if_needed()
        self.__dump_catalog_if_needed()
        self.__cleanup_if_needed()

        client = WDGridImages(self.user)
        catalog = client.get_images()

        executions = self._get_executions_proto()
        self._request_provision(executions, client, catalog)
        self._fill_webdriver_endpoints(executions)

        self.engine.config[ScenarioExecutor.EXEC] = executions

        super(WDGridProvisioning, self).prepare()

    def startup(self):
        vncs = []
        for executor in self.executors:
            if self.GRID not in executor.execution:
                continue

            grid_config = executor.execution[self.GRID][0]
            if grid_config.get('vnc', False):
                parsed = urlparse.urlparse(executor.execution['webdriver-address'])
                label = "%s - %s - %s" % (executor.label, grid_config['platform'], grid_config['browser'])
                vncs.append((parsed.netloc.split(':')[0], 'secret', label, 0))

        if vncs:
            self._vncs_pool = multiprocessing.Pool(len(vncs), maxtasksperchild=1)
            self._vncs_pool.map_async(vncviewer.main, vncs)
        super(WDGridProvisioning, self).startup()

    def shutdown(self):
        super(WDGridProvisioning, self).shutdown()
        if self._vncs_pool:
            self._vncs_pool.close()
            self._vncs_pool.terminate()
            self._vncs_pool.join()

    def post_process(self):
        for eng in self._involved_engines:
            eng.unbook()
            if self.settings.get("auto-cleanup", False):
                eng.stop()

    def _get_executions_proto(self):
        executions = []
        proto = self.engine.config.get(ScenarioExecutor.EXEC, [])
        for execution in proto if isinstance(proto, list) else [proto]:
            if self.GRID not in execution:
                executions.append(execution)
                continue

            if not isinstance(execution[self.GRID], list):
                execution[self.GRID] = [execution[self.GRID]]

            for item in execution[self.GRID]:
                exec_item = copy.deepcopy(execution)
                exec_item[self.GRID] = [item]
                executions.append(exec_item)
        self.log.debug("Executions prototype 1: %s", to_json(executions))
        return executions

    def _request_provision(self, executions, client, catalog):
        engines = client.get_engines()
        provision_items = []
        for execution in executions:
            if self.GRID not in execution:
                continue

            grid_conf = execution[self.GRID][0]
            img = self.get_image_gridspec(grid_conf, catalog)
            if img is None:
                raise TaurusConfigError("Grid item is invalid: %s" % grid_conf)

            grid_conf["imageId"] = img['id']
            grid_conf["engineId"] = None
            reused_engine = self._find_engine_to_reuse(engines, grid_conf["imageId"], self._get_label(execution))
            if reused_engine:
                self.log.debug("Found engine to reuse: %s", reused_engine)
                self._use_engine(execution, reused_engine)
            else:
                provision_items.append((execution, grid_conf, img))

        if provision_items:
            request = [{"name": self._get_label(exec_marker), "imageId": img['id']}
                       for exec_marker, grid_item, img in provision_items]
            self.log.debug("Items to provision via grid: %s", request)
            self._involved_engines.extend(client.provision(request))
            # TODO: maybe we should immediately book the engines we provisioned, to avoid stealing
            self._wait_provision()

        self.log.debug("Executions prototype 2: %s", to_json(executions))

    def _find_engine_to_reuse(self, engines, image_id, label):
        for engine in engines:
            if engine['imageId'] != image_id:
                continue

            if engine['status'] not in ('RUNNING',):
                continue

            if engine['bookingId']:
                continue

            engine.book("%s" % id(self), label)
            self._involved_engines.append(engine)
            return engine

        return None

    def _wait_provision(self):
        ready = lambda x: x['status'] in ("RUNNING", "STOPPING", "Terminating")
        for engine in self._involved_engines:
            if not ready(engine):
                engine.fetch()

            while not ready(engine):
                timeout = max(int(engine.timeout / 2), 1)
                self.log.info("Waiting for provisioning %ss...", timeout)
                time.sleep(timeout)
                engine.fetch()

            if not engine['bookingId']:
                engine.book("%s" % id(self))

    def _fill_webdriver_endpoints(self, executions):
        self.log.debug("Involved engines: %s", self._involved_engines)
        for engine in self._involved_engines:
            for execution in executions:
                if self.GRID not in execution:
                    continue

                grid_conf = execution[self.GRID][0]
                if not grid_conf["engineId"] and grid_conf['imageId'] == engine['imageId']:
                    self._use_engine(execution, engine)

        self.log.debug("Executions prototype 3: %s", to_json(executions))

    def _use_engine(self, execution, engine):
        execution[self.GRID][0]["engineId"] = engine['id']
        execution['webdriver-address'] = engine['endpoint']
        execution.get('capabilities', force_set=True).merge({
            "browser": execution[self.GRID][0]['browser'].split("/")[0]
        })

    def __dump_catalog_if_needed(self):
        if self.settings.get("dump-catalog", False):
            self.log.warning("Dumping available WebDriver images below:")
            client = WDGridImages(self.user)
            for img in client.get_images():
                self.log.info("Image ID: %s\tOS: %s %s\tBrowser: %s %s",
                              img['id'], img['operatingSystem'], img['operatingSystemVersion'], img['browser'],
                              img['browserVersion'], )

            raise NormalShutdown("Done listing images")

    def __dump_status_if_needed(self):
        if self.settings.get("dump-status", False):
            self.log.warning("Dumping status of WebDriver engines below:")
            client = WDGridImages(self.user)
            for eng in client.get_engines():
                self.log.info("Engine ID: %s\timageId: %s\tstatis: %s\tComment: %s",
                              eng['id'], eng['imageId'], eng['status'], eng['name'])

            raise NormalShutdown("Done listing images")

    def __cleanup_if_needed(self):
        if self.settings.get("cleanup-engines", False):
            self.log.warning("Cleaning up WebDriver engines")
            client = WDGridImages(self.user)
            for img in client.get_engines():
                if img['status'] in ('Terminating',):
                    continue
                self.log.info("Stopping: %s\t%s\t#%s", img['imageId'], img['name'], img['id'], )
                img.stop()

            raise NormalShutdown("Done cleanup")

    def get_image_gridspec(self, item, catalog):
        for img in catalog:
            platf = item['platform'].split('/')
            browser = item['browser'].split('/')
            if img['operatingSystem'] == platf[0] and img['operatingSystemVersion'] == platf[1] \
                    and img['browser'] == browser[0] and img['browserVersion'] == browser[1]:
                return img

        return None

    def _get_label(self, execution):
        label = execution.get('scenario')
        if not isinstance(label, text_type):
            label = str(id(label))
        return label
