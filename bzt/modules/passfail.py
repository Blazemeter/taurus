"""
Test failure criteria and auto-stopping

Copyright 2015 BlazeMeter Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""
import fnmatch
import logging
import re
import sys
from abc import abstractmethod
from collections import OrderedDict

from urwid import Pile, Text

from bzt import AutomatedShutdown, TaurusConfigError
from bzt.engine import Reporter, Service
from bzt.modules.aggregator import KPISet, DataPoint, AggregatorListener, ResultsProvider
from bzt.modules.console import WidgetProvider, PrioritizedWidget
from bzt.six import string_types, viewvalues, iteritems
from bzt.utils import load_class, dehumanize_time, BetterDict


class PassFailStatus(Reporter, Service, AggregatorListener, WidgetProvider):  # TODO: remove Service
    """
    :type criteria: list[FailCriterion]
    """

    def __init__(self):
        super(PassFailStatus, self).__init__()
        self.criteria = []
        self.widget = None
        self.last_datapoint = None

    def prepare(self):
        super(PassFailStatus, self).prepare()
        criteria = self.parameters.get("criteria", [])
        if isinstance(criteria, dict):
            crit_iter = iteritems(criteria)
        else:
            crit_iter = enumerate(criteria)

        for idx, crit_config in crit_iter:
            if isinstance(crit_config, string_types):
                crit_config = DataCriterion.string_to_config(crit_config)
                self.parameters['criteria'][idx] = crit_config
            crit = load_class(crit_config.get('class', DataCriterion.__module__ + "." + DataCriterion.__name__))
            crit_instance = crit(crit_config, self)
            assert isinstance(crit_instance, FailCriterion)
            if isinstance(idx, string_types):
                crit_instance.message = idx
            self.criteria.append(crit_instance)

        if isinstance(self.engine.aggregator, ResultsProvider):
            self.engine.aggregator.add_listener(self)

    def post_process(self):
        super(PassFailStatus, self).post_process()

        if self.last_datapoint is not None:
            for crit in self.criteria:
                if isinstance(crit, DataCriterion):
                    if crit.selector == DataPoint.CUMULATIVE:
                        crit.aggregated_second(self.last_datapoint)

        for crit in self.criteria:
            if isinstance(crit, DataCriterion):
                if crit.selector == DataPoint.CUMULATIVE:
                    if crit.is_triggered and crit.fail:
                        raise AutomatedShutdown("%s" % crit)
                else:
                    if crit.is_triggered and not crit.stop and crit.fail:
                        raise AutomatedShutdown("%s" % crit)

    def check(self):
        """
        Check if we should stop

        :return:
        """
        if self.widget:
            self.widget.update()
        res = super(PassFailStatus, self).check()
        for crit in self.criteria:
            res = res or crit.check()
        return res

    def aggregated_second(self, data):
        """
        Inform criteria of the data

        :type data: bzt.modules.aggregator.DataPoint
        """
        self.last_datapoint = data
        for crit in self.criteria:
            if isinstance(crit, DataCriterion):
                if crit.selector != DataPoint.CUMULATIVE:
                    crit.aggregated_second(data)

    def get_widget(self):
        """
        Add widget to console screen

        :return:
        """
        if not self.widget:
            self.widget = PassFailWidget(self)
        return self.widget


class FailCriterion(object):
    def __init__(self, config, owner):
        self.owner = owner
        self.config = config
        self.agg_buffer = OrderedDict()
        self.percentage = str(config['threshold']).endswith('%')
        self.get_value = self._get_field_functor(config['subject'], self.percentage)
        self.window_logic = config.get('logic', 'for')
        self.agg_logic = self._get_aggregator_functor(self.window_logic, config['subject'])
        self.condition = self._get_condition_functor(config.get('condition', '>', force_set=True))
        self.threshold = dehumanize_time(config['threshold'])
        self.stop = config.get('stop', True)
        self.fail = config.get('fail', True)
        self.message = config.get('message', None)
        self.window = dehumanize_time(config.get('timeframe', 0))
        self._start = sys.maxsize
        self._end = 0
        self.is_candidate = False
        self.is_triggered = False

    def __repr__(self):
        if self.is_triggered:
            if self.fail:
                state = "Failed"
            else:
                state = "Notice"
        else:
            state = "Alert"

        if self.message is not None:
            return "%s: %s" % (state, self.message)
        else:
            data = (state,
                    self.config['subject'],
                    self.config['condition'],
                    self.config['threshold'],
                    self.window_logic,
                    self.get_counting())
            return "%s: %s%s%s %s %d sec" % data

    def process_criteria_logic(self, tstmp, get_value):
        value = self.agg_logic(tstmp, get_value)
        state = self.condition(value, self.threshold)

        if self.window_logic == 'for':
            if state:
                self._start = min(self._start, tstmp)
                self._end = tstmp
            else:
                self._start = sys.maxsize
                self._end = 0

            if self.get_counting() >= self.window:
                self.trigger()
        elif self.window_logic == 'within' and state:
            self._start = tstmp - self.window + 1
            self._end = tstmp
            self.trigger()
        elif self.window_logic == 'over' and state:
            min_buffer_tstmp = min(self.agg_buffer.keys())
            self._start = min_buffer_tstmp
            self._end = tstmp
            if self.get_counting() >= self.window:
                self.trigger()

        logging.debug("%s %s: %s", tstmp, self, state)

    def trigger(self):
        if not self.is_triggered:
            logging.warning("%s", self)
        self.is_triggered = True

    def check(self):
        """
        Interrupt the execution if desired condition occured
        :raise AutomatedShutdown:
        """
        if self.stop and self.is_triggered:
            if self.fail:
                logging.info("Pass/Fail criterion triggered shutdown: %s", self)
                raise AutomatedShutdown("%s" % self)
            else:
                return True
        return False

    @abstractmethod
    def _get_field_functor(self, subject, percentage):
        pass

    def _get_condition_functor(self, cond):
        if cond == '=' or cond == '==':
            return lambda x, y: x == y
        elif cond == '>':
            return lambda x, y: x > y
        elif cond == '>=':
            return lambda x, y: x >= y
        elif cond == '<':
            return lambda x, y: x < y
        elif cond == '<=':
            return lambda x, y: x <= y
        else:
            raise TaurusConfigError("Unsupported fail criteria condition: %s" % cond)

    def _get_aggregator_functor(self, logic, _subject):
        if logic == 'for':
            return lambda tstmp, value: value
        elif logic in ('within', 'over'):
            return self._within_aggregator_avg  # FIXME: having simple average for percented values is a bit wrong
        else:
            raise TaurusConfigError("Unsupported window logic: %s" % logic)

    def _get_windowed_points(self, tstmp, value):
        self.agg_buffer[tstmp] = value
        keys = list(self.agg_buffer.keys())
        for tstmp_old in keys:
            if tstmp_old <= tstmp - self.window:
                del self.agg_buffer[tstmp_old]
                continue
            break

        return viewvalues(self.agg_buffer)

    def _within_aggregator_sum(self, tstmp, value):
        return sum(self._get_windowed_points(tstmp, value))

    def _within_aggregator_avg(self, tstmp, value):
        points = self._get_windowed_points(tstmp, value)
        return sum(points) / len(points)

    def get_counting(self):
        return self._end - self._start + 1


class DataCriterion(FailCriterion):
    """
    errors?
    duration (less or more than expected)
    errors in tools log?

    steady and threshold
    negate condition

    a way to inform other modules about the reason and mark the moment of start counting
    and trigger countdown for windowed

    :type config: dict
    :type owner: bzt.engine.EngineModule
    """

    def __init__(self, config, owner):
        super(DataCriterion, self).__init__(config, owner)
        self.label = config.get('label', '')
        self.selector = DataPoint.CURRENT if self.window > 0 else DataPoint.CUMULATIVE

    def aggregated_second(self, data):
        """
        Main criteria logic contained here

        :type data: bzt.modules.aggregator.DataPoint
        """
        part = data[self.selector]
        if self.label not in part:
            logging.debug("No label %s in %s", self.label, part.keys())
            return

        val = self.get_value(part[self.label])
        self.process_criteria_logic(data[DataPoint.TIMESTAMP], val)

    def _get_field_functor(self, subject, percentage):
        if subject == 'avg-rt':
            if percentage:
                raise TaurusConfigError("Percentage threshold is not applicable for %s" % subject)
            return lambda x: x[KPISet.AVG_RESP_TIME]
        elif subject == 'avg-lt':
            if percentage:
                raise TaurusConfigError("Percentage threshold is not applicable for %s" % subject)
            return lambda x: x[KPISet.AVG_LATENCY]
        elif subject == 'avg-ct':
            if percentage:
                raise TaurusConfigError("Percentage threshold is not applicable for %s" % subject)
            return lambda x: x[KPISet.AVG_CONN_TIME]
        elif subject == 'stdev-rt':
            if percentage:
                raise TaurusConfigError("Percentage threshold is not applicable for %s" % subject)
            return lambda x: x[KPISet.STDEV_RESP_TIME]
        elif subject.startswith('concurr'):
            if percentage:
                raise TaurusConfigError("Percentage threshold is not applicable for %s" % subject)
            return lambda x: x[KPISet.CONCURRENCY]
        elif subject == 'hits':
            if percentage:
                raise TaurusConfigError("Percentage threshold is not applicable for %s" % subject)
            return lambda x: x[KPISet.SAMPLE_COUNT]
        elif subject.startswith('succ'):
            if percentage:
                return lambda x: 100.0 * x[KPISet.SUCCESSES] / x[KPISet.SAMPLE_COUNT]
            else:
                return lambda x: x[KPISet.SUCCESSES]
        elif subject.startswith('fail'):
            if percentage:
                return lambda x: 100.0 * x[KPISet.FAILURES] / x[KPISet.SAMPLE_COUNT]
            else:
                return lambda x: x[KPISet.FAILURES]
        elif subject.startswith('p'):
            if percentage:
                raise TaurusConfigError("Percentage threshold is not applicable for %s" % subject)
            level = str(float(subject[1:]))
            return lambda x: x[KPISet.PERCENTILES][level] if level in x[KPISet.PERCENTILES] else 0
        elif subject.startswith('rc'):
            count = lambda x: sum([
                                      x[KPISet.RESP_CODES][y]
                                      for y in x[KPISet.RESP_CODES].keys()
                                      if fnmatch.fnmatch(y, subject[2:])
                                      ])
            if percentage:
                return lambda x: 100.0 * count(x) / float(x[KPISet.SAMPLE_COUNT])
            else:
                return count
        else:
            raise TaurusConfigError("Unsupported fail criteria subject: %s" % subject)

    def _get_aggregator_functor(self, logic, subj):
        if logic in ('within', "over") and not self.percentage:
            if subj in ('hits',) or subj.startswith('succ') or subj.startswith('fail') or subj.startswith('rc'):
                return self._within_aggregator_sum

        return super(DataCriterion, self)._get_aggregator_functor(logic, subj)

    @staticmethod
    def string_to_config(crit_config):
        """
        Parse string like "avg-rt of label>100ms for 1m, continue as non-failed"
        into config dict

        :type crit_config: str
        :rtype: dict
        """
        res = BetterDict()
        res.merge({
            "subject": None,
            "condition": None,
            "threshold": None,
            "logic": "for",
            "timeframe": 0,
            "label": "",
            "stop": True,
            "fail": True,
            "message": None,
        })

        if ':' in crit_config:
            res['message'] = crit_config[:crit_config.index(':')].strip()
            crit_config = crit_config[crit_config.index(':') + 1:].strip()

        if ',' in crit_config:
            crit_str = crit_config[:crit_config.index(',')].strip()
            action_str = crit_config[crit_config.index(',') + 1:].strip()
        else:
            crit_str = crit_config
            action_str = ""

        crit_pat = re.compile(r"([\w?*.-]+)(\s*of\s*([\S ]+))?\s*([<>=]+)\s*(\S+)(\s+(for|within|over)\s+(\S+))?")
        crit_match = crit_pat.match(crit_str.strip())
        if not crit_match:
            raise TaurusConfigError("Criteria string is malformed in its condition part: %s" % crit_str)
        crit_groups = crit_match.groups()
        res["subject"] = crit_groups[0]
        res["condition"] = crit_groups[3]
        res["threshold"] = crit_groups[4]
        if crit_groups[2]:
            res["label"] = crit_groups[2]
        if crit_groups[6]:
            res["logic"] = crit_groups[6]
        if crit_groups[7]:
            res["timeframe"] = crit_groups[7]

        if action_str:
            action_pat = re.compile(r"(stop|continue)(\s+as\s+(failed|non-failed))?")
            act_match = action_pat.match(action_str.strip())
            if not act_match:
                raise TaurusConfigError("Criteria string is malformed in its action part: %s" % action_str)
            action_groups = act_match.groups()
            res["stop"] = action_groups[0] != "continue"
            res["fail"] = action_groups[2] is None or action_groups[2] == "failed"

        return res


class PassFailWidget(Pile, PrioritizedWidget):
    """
    Represents console widget for pass/fail criteria visualisation
    If criterion is failing, it will be displayed on the widget
    return urwid widget
    :type failing_criteria: list[FailCriterion]
    """

    def __init__(self, pass_fail_reporter):
        self.pass_fail_reporter = pass_fail_reporter
        self.failing_criteria = []
        self.text_widget = Text("")
        super(PassFailWidget, self).__init__([self.text_widget])
        PrioritizedWidget.__init__(self)

    def __prepare_colors(self):
        """
        returns tuple ("color", text)
        :return:
        """
        result = []
        for failing_criterion in self.failing_criteria:
            if failing_criterion.window:
                percent = failing_criterion.get_counting() / failing_criterion.window
            else:
                percent = 1
            color = 'stat-txt'
            if 0.5 <= percent < 0.8:
                color = 'pf-3'
            elif 0.8 <= percent < 1:
                color = 'pf-4'
            elif 1 <= percent:  # pylint: disable=misplaced-comparison-constant
                color = 'pf-5'
            result.append((color, "%s\n" % failing_criterion))

        return result

    def update(self):
        """
        updates widget text
        :return:
        """
        self.text_widget.set_text("")
        self.failing_criteria = [x for x in self.pass_fail_reporter.criteria if x.get_counting() > 0]
        if self.failing_criteria:
            widget_text = self.__prepare_colors()
            self.text_widget.set_text(widget_text)
        self._invalidate()
