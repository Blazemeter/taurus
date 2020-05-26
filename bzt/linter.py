import traceback

from bzt.utils import iteritems


def dameraulevenshtein(seq1, seq2):
    """Calculate the Damerau-Levenshtein distance between sequences.

    This distance is the number of additions, deletions, substitutions,
    and transpositions needed to transform the first sequence into the
    second. Although generally used with strings, any sequences of
    comparable objects will work.

    Transpositions are exchanges of *consecutive* characters; all other
    operations are self-explanatory.

    This implementation is O(N*M) time and O(M) space, for N and M the
    lengths of the two sequences.

    Written by Michael Homer
    """
    oneago = None
    thisrow = list(range(1, len(seq2) + 1)) + [0]
    seq1_size = len(seq1)
    seq2_size = len(seq2)
    for x in range(seq1_size):
        twoago, oneago, thisrow = oneago, thisrow, [0] * len(seq2) + [x + 1]
        for y in range(seq2_size):
            delcost = oneago[y] + 1
            addcost = thisrow[y - 1] + 1
            subcost = oneago[y - 1] + (seq1[x] != seq2[y])
            thisrow[y] = min(delcost, addcost, subcost)
            if (x > 0 and y > 0 and seq1[x] == seq2[y - 1]
                and seq1[x-1] == seq2[y] and seq1[x] != seq2[y]):
                thisrow[y] = min(thisrow[y], twoago[y - 2] + 1)
    return thisrow[len(seq2) - 1]


def most_similar_string(key, variants):
    return min([(dameraulevenshtein(key, v), v) for v in variants], key=lambda dv: dv[0])


class Path(object):
    def __init__(self, *components):
        self.components = list(components)

    def add_component(self, part):
        self.components.append(part)

    def matches(self, another):
        if len(self.components) != len(another.components):
            return False
        for lpart, rpart in zip(self.components, another.components):
            if lpart == rpart:
                continue
            elif lpart == "*" or rpart == "*":
                continue
            else:
                return False
        return True

    def __eq__(self, another):
        if len(self.components) != len(another.components):
            return False
        for lpart, rpart in zip(self.components, another.components):
            if lpart != rpart:
                return False
        return True

    def __iter__(self):
        return iter(self.components)

    def copy(self):
        cmps = [cmp for cmp in self.components]
        return Path(*cmps)

    def __str__(self):
        return "%s" % ".".join(str(part) for part in self.components)

    def __repr__(self):
        return "Path(%s)" % ".".join(str(part) for part in self.components)

    def __hash__(self):
        return hash(tuple(self.components))

    def is_concrete(self):
        return all(part != "*" for part in self.components)

    def is_masked(self):
        return any(part == "*" for part in self.components)


class ConfigWarning(object):
    ERROR = "Error"
    WARNING = "Warning"

    def __init__(self, severity, identifier, path, message):
        self.severity = severity
        self.identifier = identifier
        self.path = path
        self.message = message

    def __str__(self):
        return "at path '%s': %s" % (self.path, self.message)


class ConfigurationLinter(object):
    def __init__(self, config, ignored_warnings, parent_log):
        """

        :type config: dict
        :type ignored_warnings: list[str]
        """
        self.log = parent_log.getChild(self.__class__.__name__)
        self._subscriptions = {}
        self._warnings = []
        self._config = config
        self._checkers = []
        self._ignored_warnings = ignored_warnings

    def register_checkers(self):
        self._checkers = [
            ExecutionChecker(self),
            ToplevelChecker(self),
            ScenarioChecker(self),
            JMeterScenarioChecker(self),
        ]

    def subscribe(self, path, sub):
        if path in self._subscriptions:
            self._subscriptions[path].append(sub)
        else:
            self._subscriptions[path] = [sub]

    def report_warning(self, warning):
        if warning.identifier not in self._ignored_warnings:
            self._warnings.append(warning)

    def run_subscribers(self, concrete_path, value):
        for sub_path, sub_funs in iteritems(self._subscriptions):
            if sub_path.matches(concrete_path):
                for fun in sub_funs:
                    try:
                        fun(concrete_path, value)
                    except BaseException:
                        self.log.warning("Checker failed: %s", traceback.format_exc())
                        continue

    def get_config_value(self, path, raise_if_not_found=True):
        if not path.is_concrete():
            raise ValueError("Can't access config by masked path: %s" % path)

        cfg = self._config
        for key in path:
            if key not in cfg:
                if raise_if_not_found:
                    raise ValueError("Key not found: %r" % key)
                else:
                    return None
            cfg = cfg[key]
        return cfg

    def lint(self):
        init_path = Path()
        self.visit(init_path, self._config)

    def get_warnings(self):
        return self._warnings

    def visit(self, path, value):
        self.run_subscribers(path, value)
        if isinstance(value, dict):
            self.visit_dict(path, value)
        elif isinstance(value, list):
            self.visit_list(path, value)

    def visit_dict(self, path, value):
        keys = list(value.keys())
        for key in sorted(keys):
            new_path = path.copy()
            new_path.add_component(key)
            self.visit(new_path, value[key])

    def visit_list(self, path, value):
        for index, value in enumerate(value):
            new_path = path.copy()
            new_path.add_component(index)
            self.visit(new_path, value)


class Checker(object):
    def __init__(self, linter):
        """

        :type linter: ConfigurationLinter
        """
        self.linter = linter
        self.log = linter.log.getChild(self.__class__.__name__)

    def check_for_typos(self, cpath, key, variants):
        edits, suggestion = most_similar_string(key, variants)
        # NOTE: or should it be a list of suggestions?
        if edits <= 3:
            key_path = cpath.copy()
            key_path.add_component(key)
            self.report(ConfigWarning.WARNING, 'possible-typo', key_path,
                        "unfamiliar name %r. Did you mean %r?" % (key, suggestion))

    def report(self, severity, warning_id, cpath, message):
        self.linter.report_warning(ConfigWarning(severity, warning_id, cpath, message))


class ExecutionChecker(Checker):
    def __init__(self, linter):
        super(ExecutionChecker, self).__init__(linter)
        self.linter.subscribe(Path("execution"), self.on_execution)

    def on_execution(self, cpath, value):
        if isinstance(value, list):
            for index, item in enumerate(value):
                path = cpath.copy()
                path.add_component(index)
                self.on_execution_item(path, item)
        else:
            if isinstance(value, dict):
                self.report(ConfigWarning.WARNING, 'single-execution', cpath, "'execution' should be a list")
                self.on_execution_item(cpath, value)
            else:
                self.report(ConfigWarning.ERROR, 'execution-non-list', cpath, "'execution' should be a list")

    def on_execution_item(self, cpath, execution):
        known_fields = [
            "concurrency", "iterations", "hold-for", "ramp-up", "steps", "throughput", "files", "scenario"
        ]
        if not isinstance(execution, dict):
            return
        if "scenario" not in execution:
            if "executor" not in execution or execution["executor"] != "external-results-loader":
                self.report(ConfigWarning.ERROR, 'no-scenario', cpath, "execution item doesn't specify scenario")
        for field in execution:
            if field not in known_fields:
                self.check_for_typos(cpath, field, known_fields)
            # TODO: check value type for all values (e.g. int|timestamp for hold-for, int for concurrency, etc)?


class ToplevelChecker(Checker):
    KNOWN_TOPLEVEL_SECTIONS = [
        "cli", "cli-aliases", "execution", "install-id", "modules", "provisioning", "reporting", "scenarios",
        "settings", "services", "version",
        "locations", "locations-weighted",  # v2 cloud tests only
        "included-configs",
    ]

    def __init__(self, linter):
        super(ToplevelChecker, self).__init__(linter)
        self.linter.subscribe(Path(), self.on_toplevel_key)

    def on_toplevel_key(self, cpath, cfg):
        if not isinstance(cfg, dict):
            return
        for key in cfg:
            if key not in self.KNOWN_TOPLEVEL_SECTIONS:
                self.check_for_typos(cpath, key, self.KNOWN_TOPLEVEL_SECTIONS)


class ScenarioChecker(Checker):
    def __init__(self, linter):
        super(ScenarioChecker, self).__init__(linter)
        self.linter.subscribe(Path("scenarios", "*"), self.on_named_scenario)
        self.linter.subscribe(Path("execution", "*", "scenario"), self.on_execution_scenario)

    def on_named_scenario(self, cpath, scenario):
        if isinstance(scenario, dict):
            self.check_scenario(cpath, scenario)
        else:
            self.report(ConfigWarning.ERROR, 'scenario-non-dict', cpath, "scenario is not a dict")

    def on_execution_scenario(self, cpath, scenario):
        if isinstance(scenario, dict):
            self.check_scenario(cpath, scenario)
        elif isinstance(scenario, str):
            scenario_name = scenario
            scenario_path = Path("scenarios", scenario_name)
            scenario = self.linter.get_config_value(scenario_path, raise_if_not_found=False)
            if not scenario:
                self.report(ConfigWarning.ERROR, 'undefined-scenario', cpath,
                            "scenario %r is used but isn't defined" % scenario_name)

    def check_scenario(self, cpath, scenario):
        if "script" not in scenario and "requests" not in scenario:
            self.report(ConfigWarning.ERROR, 'no-script-or-requests', cpath,
                        "scenario doesn't define neither 'script' nor 'requests'")
        elif "script" in scenario and "requests" in scenario:
            self.report(ConfigWarning.WARNING, 'script-and-requests', cpath,
                        "scenario defines both 'script' and 'requests'")


class JMeterScenarioChecker(Checker):
    def __init__(self, linter):
        super(JMeterScenarioChecker, self).__init__(linter)
        self.linter.subscribe(Path("execution", "*"), self.on_execution_item)

    def get_named_scenario(self, scenario_name):
        return self.linter.get_config_value(Path("scenarios", scenario_name), raise_if_not_found=False)

    def on_execution_item(self, cpath, execution):
        if not isinstance(execution, dict):
            return
        if "executor" in execution and execution.get("executor") != "jmeter":
            return
        scenario = execution.get("scenario", None)
        if not scenario:
            return
        if isinstance(scenario, str):
            scenario_name = scenario
            scenario = self.get_named_scenario(scenario_name)
            if not scenario:
                scenario = None
            scenario_path = Path("scenarios", scenario_name)
        else:
            scenario_path = cpath.copy()
            scenario_path.add_component("scenario")

        if scenario is not None:
            self.check_jmeter_scenario(scenario_path, scenario)

    def check_jmeter_scenario(self, cpath, scenario):
        known_fields = [
            "script", "requests", "retrieve-resources", "variables", "modifications", "properties", "headers",
            "think-time", "timeout", "default-address", "keepalive", "follow-redirects", "data-sources", "cookies"
        ]
        if not isinstance(scenario, dict):
            return
        for key in scenario:
            if key not in known_fields:
                self.check_for_typos(cpath, key, known_fields)
