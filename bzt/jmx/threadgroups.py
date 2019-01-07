from bzt.jmx.base import JMX

GETTING_PARAM_ERR_MSG = "{tg}: getting of {name} is impossible ({params})"


class AbstractThreadGroup(object):
    XPATH = None
    RAMP_UP_SEL = None
    CONCURRENCY_SEL = None

    def __init__(self, element, logger):
        self.element = element
        self.gtype = self.__class__.__name__
        self.log = logger.getChild(self.gtype)

    def get_testname(self):
        return self.element.get('testname')

    def set_concurrency(self, concurrency=None):
        self.log.warning('Setting of concurrency for %s not implemented', self.gtype)

    def set_ramp_up(self, ramp_up=None):
        self.log.warning('Setting of ramp-up for %s not implemented', self.gtype)

    def get_duration(self):
        """
        task duration or None if getting isn't possible (skipped, timeless, jmeter variables, etc.)
        """
        self.log.warning('Getting of duration for %s not implemented', self.gtype)

    def get_rate(self, raw=False):
        self.log.warning('Getting of rate for %s not implemented', self.gtype)

    def get_iterations(self):
        """
        iterations number or None if getting isn't possible (skipped, unsupported, jmeter variables, etc.)
        Note: ConcurrencyThreadGroup and ArrivalsThreadGroup aren't stopped by iterations limit
        """
        self.log.warning('Getting of iterations for %s not implemented', self.gtype)

    def get_ramp_up(self, raw=False):
        return self._get_val(self.RAMP_UP_SEL, name='ramp-up', default=1, raw=raw)

    def get_concurrency(self, raw=False):
        return self._get_val(self.CONCURRENCY_SEL, name='concurrency', default=1, raw=raw)

    def _get_val(self, selector, name='', default=None, raw=False):
        if not selector:
            self.log.warning(GETTING_PARAM_ERR_MSG.format(tg=self.gtype, name=name, params="not implemented"))
            return default

        element = self.element.find(selector)
        if element is None:
            raw_val = None
        else:
            raw_val = element.text

        if raw:
            return raw_val

        try:
            return int(raw_val)
        except (ValueError, TypeError):
            msg = "Parsing {param} '{val}' in group '{gtype}' failed, choose {default}"
            self.log.warning(msg.format(param=name, val=raw_val, gtype=self.gtype, default=default))
            return default

    def get_on_error(self):
        action = self.element.find(".//stringProp[@name='ThreadGroup.on_sample_error']")
        if action is not None:
            return action.text


class ThreadGroup(AbstractThreadGroup):
    XPATH = 'jmeterTestPlan>hashTree>hashTree>ThreadGroup'
    CONCURRENCY_SEL = ".//*[@name='ThreadGroup.num_threads']"
    RAMP_UP_SEL = ".//*[@name='ThreadGroup.ramp_time']"

    def get_duration(self):
        sched_sel = ".//*[@name='ThreadGroup.scheduler']"
        scheduler = self._get_val(sched_sel, "scheduler", raw=True)

        if scheduler == 'true':
            duration_sel = ".//*[@name='ThreadGroup.duration']"
            return self._get_val(duration_sel, "duration")
        elif scheduler == 'false':
            return self._get_val(self.RAMP_UP_SEL, "ramp-up")
        else:
            msg = 'Getting of ramp-up for %s is impossible due to scheduler: %s'
            self.log.warning(msg, (self.gtype, scheduler))

    def get_iterations(self):
        loop_control_sel = ".//*[@name='LoopController.continue_forever']"
        loop_controller = self._get_val(loop_control_sel, name="loop controller", raw=True)
        if loop_controller == "false":
            loop_sel = ".//*[@name='LoopController.loops']"
            return self._get_val(loop_sel, name="loops")
        else:
            msg = 'Getting of ramp-up for %s is impossible due to loop_controller: %s'
            self.log.warning(msg, (self.gtype, loop_controller))

    def get_thread_delay(self):
        delay_sel = ".//*[@name='ThreadGroup.delayedStart']"
        delay = self._get_val(delay_sel, name="delay", raw=True)
        return delay == "true"

    def get_scheduler_delay(self):
        delay_sel = ".//*[@name='ThreadGroup.delay']"
        return self._get_val(delay_sel, name="delay", raw=True)


class SteppingThreadGroup(AbstractThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>kg\.apc\.jmeter\.threads\.SteppingThreadGroup'
    CONCURRENCY_SEL = ".//*[@name='ThreadGroup.num_threads']"


class UltimateThreadGroup(AbstractThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>kg\.apc\.jmeter\.threads\.UltimateThreadGroup'


# parent of ConcurrencyThreadGroup and ArrivalThreadGroup
class AbstractDynamicThreadGroup(AbstractThreadGroup):
    RAMP_UP_SEL = ".//*[@name='RampUp']"

    def _get_time_unit(self):
        unit_sel = ".//*[@name='Unit']"
        return self._get_val(unit_sel, name="unit", raw=True)

    def set_ramp_up(self, ramp_up=None):
        ramp_up_element = self.element.find(self.RAMP_UP_SEL)
        ramp_up_element.text = str(ramp_up)

    def get_duration(self):
        hold_sel = ".//*[@name='Hold']"

        hold = self._get_val(hold_sel, name="hold")
        ramp_up = self.get_ramp_up()

        # 'empty' means 0 sec, let's detect that
        p_hold = self._get_val(hold_sel, name="hold", raw=True)
        p_ramp_up = self.get_ramp_up(raw=True)
        if hold is None and not p_hold:
            hold = 0
        if ramp_up is None and not p_ramp_up:
            ramp_up = 0

        if hold is not None and ramp_up is not None:
            result = hold + ramp_up
            if self._get_time_unit() == 'M':
                result *= 60

            return result

    def get_iterations(self):
        iter_sel = ".//*[@name='Iterations']"
        return self._get_val(iter_sel, name="iterations")


class ConcurrencyThreadGroup(AbstractDynamicThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>com\.blazemeter\.jmeter\.threads\.concurrency\.ConcurrencyThreadGroup'
    CONCURRENCY_SEL = ".//*[@name='TargetLevel']"

    def set_concurrency(self, concurrency=None):
        concurrency_prop = self.element.find(self.CONCURRENCY_SEL)
        concurrency_prop.text = str(concurrency)


class ArrivalsThreadGroup(AbstractDynamicThreadGroup):
    XPATH = r'jmeterTestPlan>hashTree>hashTree>com\.blazemeter\.jmeter\.threads\.arrivals\.ArrivalsThreadGroup'
    RATE_SEL = ".//*[@name='TargetLevel']"

    def get_rate(self, raw=False):
        return self._get_val(self.RATE_SEL, name='rate', default=1, raw=raw)

    def set_rate(self, rate=None):
        rate_prop = self.element.find(self.RATE_SEL)
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

    def convert(self, source, target_gtype, load, concurrency):
        """
        Convert a thread group to ThreadGroup/ConcurrencyThreadGroup for applying of load
        """
        msg = "Converting %s (%s) to %s and apply load parameters"
        self.log.debug(msg, source.gtype, source.get_testname(), target_gtype)
        on_error = source.get_on_error()
        if not concurrency:
            concurrency = source.get_concurrency(raw=True)

        if target_gtype == ThreadGroup.__name__:
            thread_delay = None
            scheduler_delay = None
            if source.gtype == target_gtype:
                thread_delay = source.get_thread_delay()
                scheduler_delay = source.get_scheduler_delay()

            new_group_element = JMX.get_thread_group(
                concurrency=concurrency,
                rampup=load.ramp_up,
                hold=load.hold,
                iterations=load.iterations,
                testname=source.get_testname(),
                on_error=on_error,
                thread_delay=thread_delay,
                scheduler_delay=scheduler_delay)
        elif target_gtype == ConcurrencyThreadGroup.__name__:
            if source.gtype == target_gtype:
                iterations = source.get_iterations()
            else:
                iterations = ""

            new_group_element = JMX.get_concurrency_thread_group(
                concurrency=concurrency,
                rampup=load.ramp_up,
                hold=load.hold,
                steps=load.steps,
                testname=source.get_testname(),
                on_error=on_error,
                iterations=iterations)
        else:
            self.log.warning('Unsupported preferred thread group: %s', target_gtype)
            return

        source.element.getparent().replace(source.element, new_group_element)
