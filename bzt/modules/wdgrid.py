import copy
import logging
import multiprocessing
import os
import time
from struct import pack

import pygame
from vncdotool import api as vncapi
from vncdotool.client import VNCDoToolFactory, VNCDoToolClient

from bzt import TaurusConfigError, NormalShutdown, resources
from bzt.bza import User, WDGridImages
from bzt.engine import ScenarioExecutor
from bzt.modules.blazemeter import CloudProvisioning
from bzt.modules.provisioning import Local
from bzt.six import text_type, urlparse
from bzt.utils import to_json


def _start_vnc(params):
    vnc_viewer = VNCViewer(params[1])
    vnc_viewer.connect(params[0])
    while vnc_viewer.tick():
        time.sleep(0.01)


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
                vncs.append((parsed.netloc.split(':')[0], label))

        if vncs:
            self._vncs_pool = multiprocessing.Pool(len(vncs))
            self._vncs_pool.map_async(_start_vnc, vncs)
            self._vncs_pool.close()
        super(WDGridProvisioning, self).startup()

    def shutdown(self):
        super(WDGridProvisioning, self).shutdown()
        if self._vncs_pool:
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


class VNCDoToolClientExt(VNCDoToolClient, object):
    def __init__(self):
        super(VNCDoToolClientExt, self).__init__()
        self.size = None
        self.screen = None

    def _handleDecodeZRLE(self, block):
        raise NotImplementedError()

    def vncConnectionMade(self):
        VNCDoToolClient.vncConnectionMade(self)
        self.setPixelFormat()

    def commitUpdate(self, rectangles):
        VNCDoToolClient.commitUpdate(self, rectangles)
        if self.size and self.screen:
            logging.debug("Updated: %s", rectangles)
            pygame.display.update(rectangles)
            self.framebufferUpdateRequest(incremental=1)
        else:
            self.framebufferUpdateRequest(incremental=0)

    def updateRectangle(self, x, y, width, height, data):
        if not self.size:
            self.size = (width, height)  # we assume that first frame is full size
        else:
            self.screen.blit(
                pygame.image.fromstring(data, (width, height), 'RGBX'),
                (x, y)
            )

    def setPixelFormat(self, bpp=32, depth=24, bigendian=0, truecolor=1, redmax=255, greenmax=255, bluemax=255, redshift=0, greenshift=8, blueshift=16):
        pixformat = pack("!BBBBHHHBBBxxx", bpp, depth, bigendian, truecolor, redmax, greenmax, bluemax, redshift, greenshift, blueshift)
        self.transport.write(pack("!Bxxx16s", 0, pixformat))
        #rember these settings
        self.bpp, self.depth, self.bigendian, self.truecolor = bpp, depth, bigendian, truecolor
        self.redmax, self.greenmax, self.bluemax = redmax, greenmax, bluemax
        self.redshift, self.greenshift, self.blueshift = redshift, greenshift, blueshift
        self.bypp = self.bpp / 8        #calc bytes per pixel


class VNCViewer(object):
    """
    :type client: vncdotool.api.ThreadedVNCClientProxy
    """
    PROTO = VNCDoToolClientExt

    def __init__(self, title):
        super(VNCViewer, self).__init__()
        pygame.init()
        VNCDoToolFactory.protocol = self.PROTO
        self.log = logging.getLogger('')
        self.title = title
        self.client = None

    def connect(self, address, password="secret"):
        self.log.debug("Connect to " + address)
        self.client = vncapi.connect(address, password=password)
        self.client.refreshScreen()
        self._get_root_window()

    def tick(self):
        for e in pygame.event.get():
            # logging.debug("Event: %s", e)
            if e.type == pygame.QUIT:
                return False
        return True

    def _get_root_window(self):
        icon = os.path.join(os.path.dirname(os.path.abspath(resources.__file__)), "taurus.png")
        pygame.display.set_icon(pygame.image.load(icon))
        pygame.display.set_caption(self.title)
        self.client.protocol.screen = pygame.display.set_mode(self.client.protocol.size, 0, 24)


def disconnect(self):
        self.client.disconnect()

