import logging

from bzt import TaurusConfigError, NormalShutdown
from bzt.modules.linter import LinterService, ConfigurationLinter
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
        self.linter = ConfigurationLinter(self.config, [], logging.getLogger(''))
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
