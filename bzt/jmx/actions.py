from lxml import etree

from bzt import TaurusConfigError
from bzt.jmx import JMX
from bzt.jmx.tools import ProtocolHandler
from bzt.six import iteritems
from bzt.utils import dehumanize_time


class ActionsHandler(ProtocolHandler):
    def get_processor_elements(self, scenario, request):
        if 'action' in request.config:
            action = request.get('action')
            if action not in ('pause', 'stop', 'stop-now', 'continue'):
                raise TaurusConfigError("Action should be either 'pause', 'stop', 'stop-now' or 'continue'")
            target = request.get('target', 'current-thread')
            if target not in ('current-thread', 'all-threads'):
                msg = "Target for action should be either 'current-thread' or 'all-threads' but '%s' found"
                raise TaurusConfigError(msg % target)
            duration = request.get('pause-duration', None)
            if duration is not None:
                duration = dehumanize_time(duration)
            return self.compile_action_block(action, target, duration, request)
        elif 'set-variables' in request.config:
            mapping = request.get('set-variables')
            return self.compile_set_variables_block(mapping, request)

    def compile_action_block(self, action, target, duration, request):
        actions = {
            'stop': 0,
            'pause': 1,
            'stop-now': 2,
            'continue': 3,
        }
        targets = {'current-thread': 0, 'all-threads': 2}
        if action not in actions:
            raise TaurusConfigError("Action %r isn't supported, use %s", action, list(actions.keys()))
        if target not in targets:
            raise TaurusConfigError("Target %r isn't supported use %s", target, list(targets.keys()))

        action = actions[action]
        target = targets[target]
        dur = 0
        if duration is not None:
            dur = int(duration * 1000)

        test_action = JMX._get_action_block(action, target, dur)
        children = etree.Element("hashTree")
        self._add_jsr_elements(children, request)
        return [test_action, children]

    def compile_set_variables_block(self, mapping, request):
        # pause current thread for 0s
        test_action = JMX._get_action_block(action_index=1, target_index=0, duration_ms=0)
        children = etree.Element("hashTree")
        fmt = "vars.put('%s', %r);"
        request.config["jsr223"] = [{
            "language": "groovy",
            "execute": "before",
            "script-text": "\n".join(fmt % (var, expr) for var, expr in iteritems(mapping))
        }]
        self._add_jsr_elements(children, request)
        return [test_action, children]
