from collections import defaultdict

from bzt import NormalShutdown, TaurusConfigError
from bzt.engine import Service
from bzt.six import iteritems
from bzt.utils import load_class


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
    for x in range(len(seq1)):
        twoago, oneago, thisrow = oneago, thisrow, [0] * len(seq2) + [x + 1]
        for y in range(len(seq2)):
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


class LinterService(Service):
    def __init__(self):
        super(LinterService, self).__init__()
        self.warn_on_unfamiliar_fields = True
        self.linter = None

    def _get_conf_option(self, key, default=None):
        return self.parameters.get(key, self.settings.get(key, default))

    def prepare(self):
        if self.settings.get("disable", False):
            return
        self.log.info("Linting config")
        self.warn_on_unfamiliar_fields = self._get_conf_option("warn-on-unfamiliar-fields", True)
        self.linter = ConfigurationLinter(self.engine.config, self.log)
        checkers_repo = self.settings.get("checkers")
        checkers_enabled = self.settings.get("checkers-enabled", [])
        self.linter.register_checkers(checkers_repo, checkers_enabled)
        self.linter.lint()
        warnings = self.linter.get_warnings()
        for warning in warnings:
            self.log.warning(str(warning))

        if self._get_conf_option("lint-and-exit", False):
            if warnings:
                raise TaurusConfigError("There were a few errors found in the configuration.")
            else:
                raise NormalShutdown("Linting finished. No errors found.")


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


class Warning(object):
    def __init__(self, path, message):
        self.path = path
        self.message = message

    def __str__(self):
        return "Warning at path '%s': %s" % (self.path, self.message)


class ConfigurationLinter(object):
    def __init__(self, config, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self._subscriptions = {}
        self._warnings = []
        self.config = config
        self.checkers = []

    def register_checkers(self, checkers_repo, checkers_enabled):
        for checker_name in checkers_enabled:
            checker_fqn = checkers_repo.get(checker_name)
            checker_class = load_class(checker_fqn)
            checker = checker_class(self)
            self.checkers.append(checker)

    def subscribe(self, path, function):
        if path in self._subscriptions:
            self._subscriptions[path].append(function)
        else:
            self._subscriptions[path] = [function]

    def report_warning(self, warning):
        self._warnings.append(warning)

    def run_subscribers(self, concrete_path, value):
        for sub_path, sub_funs in iteritems(self._subscriptions):
            if sub_path.matches(concrete_path):
                for fun in sub_funs:
                    self.log.debug("Launching func %s at %s", fun, value)
                    fun(concrete_path, value)

    def get_config_value(self, path):
        if not path.is_concrete():
            raise ValueError("Can't access config by masked path: %s" % path)

        cfg = self.config
        for key in path:
            if key not in cfg:
                raise ValueError("Key not found: %r" % key)
            cfg = cfg[key]
        return cfg

    def lint(self):
        init_path = Path()
        self.visit(init_path, self.config)

    def get_warnings(self):
        return self._warnings

    def visit(self, path, value):
        self.log.debug("Visiting value at %s", path)
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

    def report(self, warning):
        self.linter.report_warning(warning)


class ExecutionChecker(Checker):
    def __init__(self, linter):
        super(ExecutionChecker, self).__init__(linter)
        self.linter.subscribe(Path("execution"), self.on_execution)
        self.linter.subscribe(Path("execution", "*"), self.on_execution_item)

    def on_execution(self, cpath, value):
        if not isinstance(value, list):  # execution is dict. again
            self.report(Warning(cpath, "'execution' is not a list"))

    def on_execution_item(self, cpath, value):
        # TODO: check execution items (concurrency, executor, iterations, etc)
        pass


class ToplevelChecker(Checker):
    KNOWN_TOPLEVEL_SECTIONS = [
        "cli-aliases", "execution", "install-id", "modules", "provisioning", "reporting", "scenarios",
        "settings", "services", "version",
    ]

    def __init__(self, linter):
        super(ToplevelChecker, self).__init__(linter)
        self.linter.subscribe(Path(), self.on_toplevel_key)

    def on_toplevel_key(self, cpath, cfg):
        if not isinstance(cfg, dict):
            return
        for key in cfg:
            if key not in self.KNOWN_TOPLEVEL_SECTIONS:
                edits, suggestion = most_similar_string(key, self.KNOWN_TOPLEVEL_SECTIONS)
                # NOTE: or should it be a list of suggestions?
                if edits <= 3:
                    self.report(Warning(cpath, "Unfamiliar toplevel key %r. Did you mean %r?" % (key, suggestion)))
                else:
                    self.report(Warning(cpath, "Unfamiliar toplevel key %r" % key))
