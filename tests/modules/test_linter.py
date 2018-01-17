import logging

from bzt import TaurusConfigError, NormalShutdown
from bzt.modules.linter import LinterService, ConfigurationLinter, ConfigWarning, Path
from bzt.utils import BetterDict
from tests import BZTestCase
from tests.mocks import EngineEmul


class TestLinterService(BZTestCase):
    def setUp(self):
        super(TestLinterService, self).setUp()
        self.obj = LinterService()
        self.obj.engine = EngineEmul()
        self.obj.settings.merge({
            "checkers": {
                "execution": "bzt.modules.linter.ExecutionChecker",
                "toplevel": "bzt.modules.linter.ToplevelChecker",
                "scenario": "bzt.modules.linter.ScenarioChecker",
                "scenario-jmeter": "bzt.modules.linter.JMeterScenarioChecker",
            },
            "checkers-enabled": ["execution", "toplevel", "scenario", "scenario-jmeter"],
        })

    def test_normal(self):
        self.obj.settings.merge({"lint-and-exit": True})
        self.obj.engine.config.merge({"execution": [{"concurrency": 10, "scenario": {"script": "foo.jmx"}}]})
        self.assertRaises(NormalShutdown, self.obj.prepare)

    def test_single_execution(self):
        self.obj.settings.merge({"lint-and-exit": True})
        self.obj.engine.config.merge({"execution": {"concurrency": 10, "scenario": {"script": "foo.jmx"}}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_ignore(self):
        self.obj.settings.merge({"lint-and-exit": True, "ignore-warnings": ["single-execution"]})
        self.obj.engine.config.merge({"execution": {"concurrency": 10, "scenario": {"script": "foo.jmx"}}})
        self.assertRaises(NormalShutdown, self.obj.prepare)


class TestLinter(BZTestCase):
    def setUp(self):
        super(TestLinter, self).setUp()
        self.config = BetterDict()
        self.ignored_warnings = []
        self.linter = ConfigurationLinter(self.config, self.ignored_warnings, logging.getLogger(''))
        self.linter.register_checkers({
            "execution": "bzt.modules.linter.ExecutionChecker",
            "toplevel": "bzt.modules.linter.ToplevelChecker",
            "scenario": "bzt.modules.linter.ScenarioChecker",
            "scenario-jmeter": "bzt.modules.linter.JMeterScenarioChecker",
        }, ["execution", "toplevel", "scenario", "scenario-jmeter"])

    def test_single_execution(self):
        self.config.merge({
            "execution": {
                "scenario": {
                    "script": "foo",
                }
            }
        })
        self.linter.lint()
        warnings = self.linter.get_warnings()
        self.assertEqual(warnings[0].identifier, 'single-execution')

    def test_execution_no_scenario(self):
        self.config.merge({
            "execution": [{
                "concurrency": 10,
                "hold-for": "30s",
            }]
        })
        self.linter.lint()
        warnings = self.linter.get_warnings()
        self.assertEqual(warnings[0].identifier, 'no-scenario')

    def test_execution_typos(self):
        self.config.merge({
            "execution": [{
                "concurency": 10,
                "hold-fro": "30s",
                "scenario": {
                    "script": "foo",
                }
            }]
        })
        self.linter.lint()
        warnings = self.linter.get_warnings()
        ident_msg = sorted([(warning.identifier, warning.message) for warning in warnings])
        self.assertEqual(ident_msg[0], ('possible-typo', "unfamiliar name 'concurency'. Did you mean 'concurrency'?"))
        self.assertEqual(ident_msg[1], ('possible-typo', "unfamiliar name 'hold-fro'. Did you mean 'hold-for'?"))

    def test_ignore_warnings(self):
        self.config.merge({
            "execution": [{
                "concurency": 10,  # typo
                "scenario": {
                    "script": "foo",
                }
            }]
        })
        self.ignored_warnings.append('possible-typo')
        self.linter.lint()
        warnings = self.linter.get_warnings()
        self.assertTrue(not any(w.identifier == 'possible-typo' for w in warnings))

    def test_severity(self):
        self.config.merge({
            "execution": [{
                "concurency": 10,  # typo
                "scenario": {
                    # no-script-nor-requests
                }
            }]
        })
        self.linter.lint()
        messages = self.linter.get_warnings()
        warnings = [(w.identifier, str(w.path)) for w in messages if w.severity == ConfigWarning.WARNING]
        errors = [(w.identifier, str(w.path)) for w in messages if w.severity == ConfigWarning.ERROR]
        self.assertEqual(warnings, [('possible-typo', 'execution.0.concurency')])
        self.assertEqual(errors, [('no-script-or-requests', 'execution.0.scenario')])
