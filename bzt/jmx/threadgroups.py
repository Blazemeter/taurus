from bzt.jmx.base import JMX
from bzt.requests_model import has_variable_pattern

GETTING_PARAM_ERR_MSG = "{tg}: Getting of {name} is impossible: {params}"


class AbstractThreadGroup(object):
    XPATH = None

    def __init__(self, element, logger):
        self.element = element
        self.test_name = self.element.get("testname")
        self.gtype = self.__class__.__name__
        self.log = logger.getChild(self.gtype)

        # default getter: _get_val
        # default selector: None (means "non-implemented")
        self.fields = {
            "concurrency": {"default": 1},  # single bot
            "ramp-up": {"default": 0},  # without ramp-up
            "duration": {"default": 0},  # infinite
            "iterations": {"default": 0},  # infinite
            "rate": {"default": 1},
            "hold": {"default": 0},  # infinite
            "unit": {"default": "S"},
            "steps": {"default": 1},
            "on-error": {"selector": ".//stringProp[@name='ThreadGroup.on_sample_error']", "raw": True}}

    def get(self, name, raw=False):
        parser = self.fields.get(name, {})
        params = {key: parser[key] for key in parser.keys() if key != "getter"}

        if not parser.get("raw"):  # use configured 'raw' if exist
            params["raw"] = raw

        getter = parser.get("getter")
        if not getter:
            getter = self._get_val
            params["name"] = name

        return getter(**params)

    def get_seconds(self, name, raw=False):
        """ convert time (hold, ramp-up) to seconds according to group time unit (M/S) """
        time = self.get(name, raw)
        if time and self.get("unit") == "M":
            if isinstance(time, int):
                time *= 60
            else:
                self.log.warning("Impossible to convert to seconds: %s", time)

        return time

    def _get_val(self, name, selector=None, default=None, raw=False):
        if not selector:
            self.log.debug('Getting of %s for %s not implemented', name, self.gtype)
            if raw:
                return None
            else:
                return default

        element = self.element.find(selector)
        if element is None:
            string_val = None
        else:
            string_val = element.text

        if raw:
            return string_val

        try:
            return int(string_val)
        except (ValueError, TypeError):
            msg = "Parsing {param} '{val}' in group '{gtype}' failed"
            self.log.warning(msg.format(param=name, val=string_val, gtype=self.gtype))

        return default


class ThreadGroup(AbstractThreadGroup):
    XPATH = 'jmeterTestPlan>hashTree>hashTree>ThreadGroup'

    def __init__(self, *args, **kwargs):
        super(ThreadGroup, self).__init__(*args, **kwargs)
        self.fields["concurrency"]["selector"] = ".//*[@name='ThreadGroup.num_threads']"
        self.fields["ramp-up"]["selector"] = ".//*[@name='ThreadGroup.ramp_time']"
        self.fields["duration"]["getter"] = self._get_duration
        self.fields["iterations"]["getter"] = self._get_iterations
        self.fields["hold"]["getter"] = self._get_hold

    def _get_hold(self, default=None, raw=False):
        if raw:
            return None

        duration = self.get("duration")
        ramp_up = self.get("ramp-up")
        if not ramp_up:
            hold = duration
        elif isinstance(duration, int) and isinstance(ramp_up, int):
            hold = duration - ramp_up
        else:
            params = "{'duration': %s, 'ramp-up': %s}" % (duration, ramp_up)
            self.log.warning(GETTING_PARAM_ERR_MSG.format(tg=self.gtype, name="hold", params=params))
            hold = default

        return hold

    def _get_duration(self, default=None, raw=False):
        sched_sel = ".//*[@name='ThreadGroup.scheduler']"
        scheduler = self._get_val("scheduler", selector=sched_sel, raw=True)

        if scheduler == 'true':
            duration_sel = ".//*[@name='ThreadGroup.duration']"
            return self._get_val("duration", selector=duration_sel, raw=raw)
        elif scheduler == 'false':
            return self.get("ramp-up", raw=raw)
        else:
            params = "{'scheduler': %s}" % scheduler
            self.log.warning(GETTING_PARAM_ERR_MSG.format(tg=self.gtype, name="duration", params=params))
            return default

    def _get_iterations(self, default=None, raw=False):
        loop_control_sel = ".//*[@name='LoopController.continue_forever']"
        loop_controller = self._get_val("loop controller", selector=loop_control_sel, raw=True)
        if loop_controller == "false":
            loop_sel = ".//*[@name='LoopController.loops']"
            return self._get_val("loops", default=default, selector=loop_sel, raw=raw)
        else:
            params = "{'loop_controller': %s}" % loop_controller
            self.log.warning(GETTING_PARAM_ERR_MSG.format(tg=self.gtype, name="hold", params=params))

        return default


class SteppingThreadGroup(AbstractThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>kg\.apc\.jmeter\.threads\.SteppingThreadGroup'

    def __init__(self, *args, **kwargs):
        super(SteppingThreadGroup, self).__init__(*args, **kwargs)
        self.fields["concurrency"]["selector"] = ".//*[@name='ThreadGroup.num_threads']"
        self.fields["hold"]["selector"] = ".//*[@name='flighttime']"


class UltimateThreadGroup(AbstractThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>kg\.apc\.jmeter\.threads\.UltimateThreadGroup'


class AbstractDynamicThreadGroup(AbstractThreadGroup):
    """ parent of ConcurrencyThreadGroup and ArrivalThreadGroup """

    def __init__(self, *args, **kwargs):
        super(AbstractDynamicThreadGroup, self).__init__(*args, **kwargs)
        self.fields["ramp-up"]["selector"] = ".//*[@name='RampUp']"
        self.fields["iterations"]["selector"] = ".//*[@name='Iterations']"
        self.fields["duration"]["getter"] = self._get_duration
        self.fields["unit"]["selector"] = ".//*[@name='Unit']"
        self.fields["unit"]["raw"] = True
        self.fields["hold"]["selector"] = ".//*[@name='Hold']"
        self.fields["steps"]["selector"] = ".//*[@name='Steps']"

    def set_ramp_up(self, ramp_up=None):
        ramp_up_element = self.element.find(self.fields["ramp-up"]["selector"])
        ramp_up_element.text = str(ramp_up)

    def _get_duration(self, default=None, raw=False):
        if raw:
            return None

        # try to get number values
        hold = self.get("hold")
        ramp_up = self.get("ramp-up")

        # 'empty' means 0 sec, let's detect that
        r_hold = self.get("hold", raw=True)
        r_ramp_up = self.get("ramp-up", raw=True)
        if has_variable_pattern(r_hold) or has_variable_pattern(r_ramp_up):
            params = "{'hold': %s, 'ramp-up': %s}" % (r_hold, r_ramp_up)
            GETTING_PARAM_ERR_MSG.format(tg=self.gtype, name="hold", params=params)
            return default
        else:
            return hold + ramp_up


class ConcurrencyThreadGroup(AbstractDynamicThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>com\.blazemeter\.jmeter\.threads\.concurrency\.ConcurrencyThreadGroup'

    def __init__(self, *args, **kwargs):
        super(ConcurrencyThreadGroup, self).__init__(*args, **kwargs)
        self.fields["concurrency"]["selector"] = ".//*[@name='TargetLevel']"

    def set_concurrency(self, concurrency=None):
        concurrency_prop = self.element.find(self.fields["concurrency"]["selector"])
        concurrency_prop.text = str(concurrency)


class ArrivalsThreadGroup(AbstractDynamicThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>com\.blazemeter\.jmeter\.threads\.arrivals\.ArrivalsThreadGroup'

    def __init__(self, *args, **kwargs):
        super(ArrivalsThreadGroup, self).__init__(*args, **kwargs)
        self.fields["rate"]["selector"] = ".//*[@name='TargetLevel']"

    def set_rate(self, rate=None):
        rate_prop = self.element.find(self.fields["rate"]["selector"])
        rate_prop.text = str(rate)


class ThreadGroupHandler(object):
    CLASSES = [ThreadGroup, SteppingThreadGroup, UltimateThreadGroup, ConcurrencyThreadGroup, ArrivalsThreadGroup]

    def __init__(self, logger, raw_load, specific_load, tg):
        self.log = logger.getChild(self.__class__.__name__)
        self.raw_load = raw_load
        self.specific_load = specific_load
        self.tg = tg

    def groups(self, jmx):
        """
        Get wrappers for thread groups that are enabled
        """
        for _class in self.CLASSES:
            for group in jmx.get(_class.XPATH):
                if group.get("enabled") != "false":
                    yield _class(group, self.log)

    @staticmethod
    def choose_val(*args):
        for arg in args:
            if isinstance(arg, tuple):
                if arg[0] is not None:
                    return arg[1]
            else:
                if arg is not None:
                    return arg

    def convert(self, source, concurrency):
        """
        Convert a 'source' to ThreadGroup/ConcurrencyThreadGroup for applying user load params
        """
        msg = "Converting %s (%s) to %s and apply load parameters"
        self.log.debug(msg, source.gtype, source.test_name, self.tg)

        if self.tg == ThreadGroup.__name__:

            new_group_element = JMX.get_thread_group(
                concurrency=self.choose_val(concurrency, source.get("concurrency")),
                rampup=self.choose_val(
                    (self.raw_load.ramp_up, self.specific_load.ramp_up), source.get_seconds("ramp-up")),
                hold=self.choose_val((self.raw_load.hold, self.specific_load.hold), source.get_seconds("hold")),
                iterations=self.choose_val(
                    (self.raw_load.iterations, self.specific_load.iterations), source.get("iterations")),
                testname=source.test_name,
                on_error=source.get("on-error"))
        elif self.tg == ConcurrencyThreadGroup.__name__:
            unit = source.get("unit")
            ramp_up = self.raw_load.ramp_up
            hold = self.raw_load.hold
            if ramp_up:
                if unit == "M":
                    if has_variable_pattern(ramp_up):
                        self.log.warning("Impossible to convert property '%s' to minutes" % ramp_up)
                    else:
                        ramp_up = int(round(self.specific_load.ramp_up / 60)) or 1
                else:
                    ramp_up = self.specific_load.ramp_up

            if hold:
                if unit == "M":
                    if has_variable_pattern(hold):
                        self.log.warning("Impossible to convert property '%s' to minutes" % hold)
                    else:
                        hold = int(round(self.specific_load.hold / 60)) or 1
                else:
                    hold = self.specific_load.hold

            new_group_element = JMX.get_concurrency_thread_group(
                concurrency=self.choose_val(concurrency, source.get("concurrency")),
                rampup=self.choose_val(ramp_up, source.get("ramp-up")),
                hold=self.choose_val(hold, source.get("hold")),
                steps=self.choose_val((self.raw_load.steps, self.specific_load.steps), source.get("steps")),
                testname=source.test_name,
                unit=unit,
                on_error=source.get("on-error"))
        else:
            self.log.warning('Unsupported preferred thread group: %s', self.tg)
            return

        source.element.getparent().replace(source.element, new_group_element)
