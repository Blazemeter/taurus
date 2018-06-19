from bzt.jmx.base import JMX


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

    def get_rate(self, pure=False):
        self.log.warning('Getting of rate for %s not implemented', self.gtype)

    def get_iterations(self):
        """
        iterations number or None if getting isn't possible (skipped, unsupported, jmeter variables, etc.)
        Note: ConcurrencyThreadGroup and ArrivalsThreadGroup aren't stopped by iterations limit
        """
        self.log.warning('Getting of iterations for %s not implemented', self.gtype)

    def get_ramp_up(self, pure=False):
        if not self.RAMP_UP_SEL:
            self.log.warning('Getting of ramp-up for %s not implemented', self.gtype)
            return 1

        return self._get_val(self.RAMP_UP_SEL, name='ramp-up', default=0, pure=pure)

    def get_concurrency(self, pure=False):
        if not self.CONCURRENCY_SEL:
            self.log.warning('Getting of concurrency for %s not implemented', self.gtype)
            return 1

        return self._get_val(self.CONCURRENCY_SEL, name='concurrency', default=1, pure=pure)

    def _get_val(self, selector, name='', default=None, convertor=int, pure=False):
        element = self.element.find(selector)
        if element is None:
            string_val = None
        else:
            string_val = element.text

        if pure:
            return string_val

        try:
            return convertor(string_val)
        except (ValueError, TypeError):
            if default:
                msg = "Parsing {param} '{val}' in group '{gtype}' failed, choose {default}"
                self.log.warning(msg.format(param=name, val=string_val, gtype=self.gtype, default=default))
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
        scheduler = self._get_val(sched_sel, "scheduler", pure=True)

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
        loop_controller = self._get_val(loop_control_sel, name="loop controller", pure=True)
        if loop_controller == "false":
            loop_sel = ".//*[@name='LoopController.loops']"
            return self._get_val(loop_sel, name="loops")
        else:
            msg = 'Getting of ramp-up for %s is impossible due to loop_controller: %s'
            self.log.warning(msg, (self.gtype, loop_controller))


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
        return self._get_val(unit_sel, name="unit", pure=True)

    def set_ramp_up(self, ramp_up=None):
        ramp_up_element = self.element.find(self.RAMP_UP_SEL)
        ramp_up_element.text = str(ramp_up)

    def get_duration(self):
        hold_sel = ".//*[@name='Hold']"

        hold = self._get_val(hold_sel, name="hold")
        ramp_up = self.get_ramp_up()

        # 'empty' means 0 sec, let's detect that
        p_hold = self._get_val(hold_sel, name="hold", pure=True)
        p_ramp_up = self.get_ramp_up(pure=True)
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

    def get_rate(self, pure=False):
        return self._get_val(self.RATE_SEL, name='rate', default=1, pure=pure)

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

    def convert(self, group, target, load, concurrency):
        """
        Convert a thread group to ThreadGroup/ConcurrencyThreadGroup for applying of load
        """
        msg = "Converting %s (%s) to %s and apply load parameters"
        self.log.debug(msg, group.gtype, group.get_testname(), target)
        on_error = group.get_on_error()

        if target == ThreadGroup.__name__:
            new_group_element = JMX.get_thread_group(
                concurrency=concurrency,
                rampup=load.ramp_up,
                hold=load.hold,
                iterations=load.iterations,
                testname=group.get_testname(),
                on_error=on_error)
        elif target == ConcurrencyThreadGroup.__name__:
            new_group_element = JMX.get_concurrency_thread_group(
                concurrency=concurrency,
                rampup=load.ramp_up,
                hold=load.hold,
                steps=load.steps,
                testname=group.get_testname(),
                on_error=on_error)
        else:
            self.log.warning('Unsupported preferred thread group: %s', target)
            return

        group.element.getparent().replace(group.element, new_group_element)
