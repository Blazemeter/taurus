import copy
import logging
import math
from collections import deque
from operator import itemgetter
from typing import List


class EngineMetricsBuffer:
    """
    Acts as a buffer for engine metrics coming from various sources. Engine metrics have the following form:
    [
        {'source': 'local', 'ts': 1678892271.3985019, 'cpu': 9.4},
        {'source': 'local', 'ts': 1678892271.3985019, 'mem': 55.6},
        {'source': 'local', 'ts': 1678892271.3985019, 'bytes-sent': 26302},
        {'source': 'local', 'ts': 1678892271.3985019, 'bytes-recv': 2485},
        {'source': 'local', 'ts': 1678892271.3985019, 'conn-all': 63},
        {'source': 'local', 'ts': 1678892271.3985019, 'disk-read': 1195099},
        {'source': 'local', 'ts': 1678892271.3985019, 'disk-write': 1044112},
        {'source': 'local', 'ts': 1678892271.3985019, 'disk-space': 54.2},
        ...
    ]
    """
    def __init__(self, max_len=500):
        self._log = logging.getLogger(self.__class__.__name__)
        self.queue = deque(maxlen=max_len)

    def record_data(self, data: List[dict]):
        if len(self.queue) + len(data) > self.queue.maxlen:
            self._log.debug(f"Engine metrics queue overflow, max size {self.queue.maxlen} reached")
        for item in data:
            self.queue.append(item)

    def get_data(self) -> List[dict]:
        return list(self.queue)

    def clear(self):
        self.queue.clear()


class HappysocksMetricsConverter:
    # mapped names similar to MonitoringBuffer.get_monitoring_json
    metrics_mapping = {'cpu': 'cpu', 'mem': 'mem', 'bytes-recv': 'network_io', 'engine-loop': 'busy_taurus',
                       'conn-all': 'connections'}

    @staticmethod
    def to_metrics_batch(raw_metrics: List[dict], session_id, master_id=None, calibration_id=None,
                         calibration_step_id=None) -> List[dict]:
        """
        Converts engine metrics from internal format where each record holds single metric value into format expected by
        happysocks service.
        """
        metrics_per_ts = dict()
        for raw_item in raw_metrics:
            raw_item_copy = copy.deepcopy(raw_item)
            source = raw_item_copy.pop('source')
            ts = math.floor(raw_item_copy.pop('ts') * 1000)
            for metric_name, metric_value in raw_item_copy.items():
                mapped_metric_name = HappysocksMetricsConverter.map_metric_name(metric_name)
                if mapped_metric_name:
                    key = frozenset({'source': source, 'ts': ts}.items())
                    metrics_bag = metrics_per_ts.get(key)
                    if not metrics_bag:
                        metrics_bag = HappysocksMetricsConverter.new_metrics_bag(source, ts, session_id, master_id,
                                                                                 calibration_id, calibration_step_id)
                        metrics_per_ts[key] = metrics_bag
                    metrics_bag['values'][mapped_metric_name] = metric_value
        return sorted(list(metrics_per_ts.values()), key=itemgetter('timestamp'))

    @staticmethod
    def map_metric_name(metric_name):
        if metric_name.lower().startswith('net'):
            return HappysocksMetricsConverter.metrics_mapping.get('bytes-recv')
        return HappysocksMetricsConverter.metrics_mapping.get(metric_name)

    @staticmethod
    def new_metrics_bag(source, ts, session_id, master_id, calibration_id, calibration_step_id):
        metrics_bag = dict()
        metrics_bag['metadata'] = {'type': 'engine-health', 'source': source, 'entityId': session_id}
        if master_id:
            metrics_bag['metadata']['masterId'] = master_id
        # if we are doing calibration then add info to metadata
        if calibration_id:
            metrics_bag['metadata']['calibrationId'] = calibration_id
        if calibration_step_id:
            metrics_bag['metadata']['calibrationStepId'] = calibration_step_id
        metrics_bag['timestamp'] = ts
        metrics_bag['values'] = dict()
        return metrics_bag
