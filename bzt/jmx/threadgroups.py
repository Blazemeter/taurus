from copy import deepcopy
from bzt.jmx.base import JMX


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
            "concurrency": {"default": 1},      # single bot
            "ramp-up": {"default": 0},          # without ramp-up
            "duration": {"default": 0},         # infinite
            "iterations": {"default": 0},       # infinite
            "rate": {"default": 1},
            "hold": {"default": 0},             # infinite
            "unit": {},
            "on-error": {"selector": ".//stringProp[@name='ThreadGroup.on_sample_error']", "pure": True}}

    def get(self, name, pure=False):
        parser = self.fields.get(name, {})
        params = deepcopy(parser)
        getter = params.get("getter")
        if getter:
            del params["getter"]
        else:
            getter = self._get_val

        if not parser.get("pure"):  # use configured pure if exist
            parser["pure"] = pure

        return getter(name, **params)

    def _get_val(self, name, selector=None, default=None, pure=False):
        if not selector:
            self.log.warning('Getting of %s for %s not implemented', name, self.gtype)
            return default

        element = self.element.find(selector)
        if element is None:
            string_val = None
        else:
            string_val = element.text

        if pure:
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

    def _get_duration(self, default=None):
        sched_sel = ".//*[@name='ThreadGroup.scheduler']"
        scheduler = self._get_val("scheduler", selector=sched_sel, pure=True)

        if scheduler == 'true':
            duration_sel = ".//*[@name='ThreadGroup.duration']"
            return self._get_val("duration", selector=duration_sel)
        elif scheduler == 'false':
            return self.get("ramp-up")
        else:
            self.log.warning('Getting of ramp-up for %s is impossible due to scheduler: %s', self.gtype, scheduler)
            return default

    def _get_iterations(self):
        loop_control_sel = ".//*[@name='LoopController.continue_forever']"
        loop_controller = self._get_val("loop controller", selector=loop_control_sel, pure=True)
        if loop_controller == "false":
            loop_sel = ".//*[@name='LoopController.loops']"
            return self._get_val("loops", selector=loop_sel)
        else:
            msg = 'Getting of ramp-up for %s is impossible due to loop_controller: %s'
            self.log.warning(msg, (self.gtype, loop_controller))


class SteppingThreadGroup(AbstractThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>kg\.apc\.jmeter\.threads\.SteppingThreadGroup'

    def __init__(self, *args, **kwargs):
        super(SteppingThreadGroup, self).__init__(*args, **kwargs)
        self.fields["concurrency"]["selector"] = ".//*[@name='ThreadGroup.num_threads']"


class UltimateThreadGroup(AbstractThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>kg\.apc\.jmeter\.threads\.UltimateThreadGroup'


class AbstractDynamicThreadGroup(AbstractThreadGroup):
    """ parent of ConcurrencyThreadGroup and ArrivalThreadGroup """
    def __init__(self, *args, **kwargs):
        super(AbstractDynamicThreadGroup, self).__init__(*args, **kwargs)
        self.fields["ramp-up"]["selector"] = ".//*[@name='RampUp']"
        self.fields["iterations"]["selector"] = ".//*[@name='Iterations']"
        self.fields["unit"]["selector"] = ".//*[@name='Unit']"
        self.fields["unit"]["pure"] = True
        self.fields["hold"]["selector"] = ".//*[@name='Hold']"

    def set_ramp_up(self, ramp_up=None):
        ramp_up_element = self.element.find(self.fields["ramp-up"]["selector"])
        ramp_up_element.text = str(ramp_up)

    def _get_duration(self):
        hold = self.get("hold")
        ramp_up = self.get("ramp-up")

        # 'empty' means 0 sec, let's detect that
        p_hold = self.get("hold", pure=True)
        p_ramp_up = self.get("ramp-up", pure=True)
        if hold is None and not p_hold:
            hold = 0
        if ramp_up is None and not p_ramp_up:
            ramp_up = 0

        if hold is not None and ramp_up is not None:
            result = hold + ramp_up
            if self.get("unit") == 'M':     # time unit
                result *= 60

            return result


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

    def __init__(self, logger):
        self.log = logger.getChild(self.__class__.__name__)

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
            if arg is not None:
                return arg

    def convert(self, source, target_gtype, load, concurrency):
        """
        Convert a 'source' to ThreadGroup/ConcurrencyThreadGroup for applying user load params
        """
        msg = "Converting %s (%s) to %s and apply load parameters"
        self.log.debug(msg, source.gtype, source.get_testname(), target_gtype)

        if target_gtype == ThreadGroup.__name__:
            new_group_element = JMX.get_thread_group(
                concurrency=self.choose_val(concurrency, source.get("concurrency")),
                rampup=self.choose_val(load.ramp_up, source.get("ramp-up")),
                hold=self.choose_val(load.hold, source.get("hold")),
                iterations=self.choose_val(load.iterations, source.get("iterations")),
                testname=source.get("testname"),
                on_error=source.get("on-error"))
        elif target_gtype == ConcurrencyThreadGroup.__name__:
            new_group_element = JMX.get_concurrency_thread_group(
                concurrency=self.choose_val(concurrency, source.get("concurrency")),
                rampup=self.choose_val(load.ramp_up, source.get("ramp-up")),
                hold=self.choose_val(load.hold, source.get("hold")),
                steps=self.choose_val(load.steps, source.get("steps")),
                testname=source.get("testname"),
                on_error=source.get("on-error"))
        else:
            self.log.warning('Unsupported preferred thread group: %s', target_gtype)
            return

        source.element.getparent().replace(source.element, new_group_element)
