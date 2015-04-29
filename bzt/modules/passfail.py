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

import urwid

from bzt import AutomatedShutdown
from bzt.engine import Reporter, AggregatorListener
from bzt.modules.aggregator import KPISet, DataPoint
from bzt.utils import load_class, dehumanize_time
from bzt.modules.console import WidgetProvider
import six


class PassFailStatus(Reporter, AggregatorListener, WidgetProvider):
    """
    :type criterias: list[FailCriteria]
    """

    def __init__(self):
        super(PassFailStatus, self).__init__()
        self.criterias = []
        self.widget = None

    def prepare(self):
        super(PassFailStatus, self).prepare()
        for idx, crit_config in enumerate(self.parameters.get("criterias", [])):
            if isinstance(crit_config, six.string_types):
                crit_config = FailCriteria.string_to_config(crit_config)
                self.parameters['criterias'][idx] = crit_config
            crit = load_class(crit_config.get('type', FailCriteria.__module__ + "." + FailCriteria.__name__))
            self.criterias.append(crit(crit_config))

    def post_process(self):
        super(PassFailStatus, self).post_process()
        for crit in self.criterias:
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
        for crit in self.criterias:
            res = res or crit.check()
        return res

    def aggregated_second(self, data):
        """
        Inform criterias of the data

        :type data: bzt.modules.aggregator.DataPoint
        """
        for crit in self.criterias:
            crit.aggregated_second(data)

    def get_widget(self):
        """
        Add widget to console screen

        :return:
        """
        if not self.widget:
            self.widget = PassFailWidget(self)
        return self.widget


class FailCriteria(object):
    """
    + response codes (masks for codes)
    + average times (full, latency, conn)
    + percentiles
    errors?
    resource metrics (engine overload)
    duration (less or more than expected)
    errors in tools log?

    + overall and by label => label
    + stop test and not (what to do then??) => stop
    cumulative and last / instant or windowed => window size
    steady and threshold
    negate condition

    + percentage and absolute count => criteria-specific
    a way to inform other modules about the reason and mark the moment of start counting
    and trigger countdown for windowed

    :type config: dict
    """

    def __init__(self, config):
        super(FailCriteria, self).__init__()
        self.config = config
        self.get_value = self.__get_field_functor(config['subject'], str(config['threshold']).endswith('%'))
        self.condition = self.__get_condition_functor(config['condition'])
        self.label = config.get('label', '')
        self.threshold = dehumanize_time(config['threshold'])
        self.stop = config['stop']
        self.fail = config['fail']

        frame = dehumanize_time(config['timeframe'])
        self.window = frame
        self.selector = DataPoint.CURRENT if frame > 0 else DataPoint.CUMULATIVE

        self.counting = 0
        self.is_candidate = False
        self.is_triggered = False
        self.started = None
        self.ended = None

    def __repr__(self):
        if self.is_triggered:
            if self.fail:
                state = "Failed"
            else:
                state = "Notice"
        else:
            state = "Alert"

        data = (state,
                self.config['subject'],
                self.config['condition'],
                self.config['threshold'],
                self.counting)
        return "%s: %s%s%s for %s sec" % data

    def __count(self, data):
        self.ended = data[DataPoint.TIMESTAMP]
        self.counting += 1
        if self.counting >= self.window:
            if not self.is_triggered:
                logging.warning("%s", self)
            self.is_triggered = True

    def aggregated_second(self, data):
        """
        Main criteria logic contained here

        :type data: bzt.modules.aggregator.DataPoint
        """
        part = data[self.selector]
        if self.label not in part:
            return
        value = self.get_value(part[self.label])

        state = self.condition(value, self.threshold)
        if not state:
            self.counting = 0
            if not self.is_triggered:
                self.started = None

            if self.selector == DataPoint.CUMULATIVE:
                self.is_triggered = False
        else:
            self.__count(data)

        logging.debug("%s %s: %s", data[DataPoint.TIMESTAMP], self, state)

    def check(self):
        """
        Interrupt the execution if desired condition occured

        :return: :raise AutomatedShutdown:
        """
        if self.stop and self.is_triggered:
            if self.fail:
                logging.info("Pass/Fail criteria triggered shutdown: %s", self)
                raise AutomatedShutdown("%s" % self)
            else:
                return True
        return False

    # TODO: support aggregative algo, maybe like 'within' instead of 'for'

    def __get_field_functor(self, subject, percentage):
        if subject == 'avg-rt':
            if percentage:
                raise ValueError("Percentage threshold is not applicable for %s" % subject)
            return lambda x: x[KPISet.AVG_RESP_TIME]
        elif subject == 'avg-lt':
            if percentage:
                raise ValueError("Percentage threshold is not applicable for %s" % subject)
            return lambda x: x[KPISet.AVG_LATENCY]
        elif subject == 'avg-ct':
            if percentage:
                raise ValueError("Percentage threshold is not applicable for %s" % subject)
            return lambda x: x[KPISet.AVG_CONN_TIME]
        elif subject == 'stdev-rt':
            if percentage:
                raise ValueError("Percentage threshold is not applicable for %s" % subject)
            return lambda x: x[KPISet.STDEV_RESP_TIME]
        elif subject.startswith('concurr'):
            if percentage:
                raise ValueError("Percentage threshold is not applicable for %s" % subject)
            return lambda x: x[KPISet.CONCURRENCY]
        elif subject == 'hits':
            if percentage:
                raise ValueError("Percentage threshold is not applicable for %s" % subject)
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
                raise ValueError("Percentage threshold is not applicable for %s" % subject)
            level = float(subject[1:])
            return lambda x: x[KPISet.PERCENTILES][level] if level in x[KPISet.PERCENTILES] else 0
        elif subject.startswith('rc'):
            count = lambda x: sum([fnmatch.fnmatch(y, subject[2:]) for y in x[KPISet.RESP_CODES].keys()])
            if percentage:
                return lambda x: 100.0 * count(x) / x[KPISet.SAMPLE_COUNT]
            else:
                return count
        else:
            raise ValueError("Unsupported fail criteria subject: %s" % subject)

    def __get_condition_functor(self, cond):
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
            raise ValueError("Unsupported fail criteria condition: %s" % cond)

    @staticmethod
    def string_to_config(crit_config):
        """
        Parse string like "avg-rt of label>100ms for 1m, continue as non-failed"
        into config dict

        :type crit_config: str
        :rtype: dict
        """
        res = {
            "subject": None,
            "condition": None,
            "threshold": None,
            "timeframe": -1,
            "label": "",
            "stop": True,
            "fail": True
        }

        crit_parts = crit_config.split(',')
        crit_str = crit_parts[0]
        if len(crit_parts) > 1:
            action_str = crit_parts[1]
        else:
            action_str = ""

        crit_pat = re.compile("([\w\?-]+)(\s*of\s*([\S ]+))?([<>=]+)(\S+)(\s+for\s+(\S+))?")
        crit_match = crit_pat.match(crit_str.strip())
        if not crit_match:
            raise ValueError("Criteria string is mailformed in its condition part: %s" % crit_str)
        crit_groups = crit_match.groups()
        res["subject"] = crit_groups[0]
        res["condition"] = crit_groups[3]
        res["threshold"] = crit_groups[4]
        if crit_groups[2]:
            res["label"] = crit_groups[2]
        if crit_groups[6]:
            res["timeframe"] = crit_groups[6]

        if action_str:
            action_pat = re.compile("(stop|continue)(\s+as\s+(failed|non-failed))?")
            act_match = action_pat.match(action_str.strip())
            if not act_match:
                raise ValueError("Criteria string is mailformed in its action part: %s" % action_str)
            action_groups = act_match.groups()
            res["stop"] = action_groups[0] == "stop"
            res["fail"] = action_groups[2] == "failed"

        return res


class PassFailWidget(urwid.Pile):
    """
    Represents console widget for pass/fail criteria visualisation
    If criteria is failing, it will be displayed on the widget
    return urwid widget
    """

    def __init__(self, pass_fail_reporter):
        self.pass_fail_reporter = pass_fail_reporter
        self.failing_criteria = []
        self.text_widget = urwid.Text("")
        super(PassFailWidget, self).__init__([self.text_widget])

    def __prepare_colors(self):
        """
        returns tuple ("color", text)
        :return:
        """
        result = []
        for failing_criteria in self.failing_criteria:
            percent = failing_criteria.counting / failing_criteria.window
            color = 'stat-txt'
            if 0.5 <= percent < 0.8:
                color = 'pf-3'
            elif 0.8 <= percent < 1:
                color = 'pf-4'
            elif 1 <= percent:
                color = 'pf-5'
            result.append((color, "%s\n" % failing_criteria))

        return result

    def update(self):
        """
        updates widget text
        :return:
        """
        self.failing_criteria = [x for x in self.pass_fail_reporter.criterias if x.counting > 0]
        if self.failing_criteria:
            widget_text = self.__prepare_colors()
            self.text_widget.set_text(widget_text)
        self._invalidate()

