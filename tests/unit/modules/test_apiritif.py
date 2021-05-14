from bzt.modules.apiritif import ApiritifNoseExecutor
from tests.unit import ExecutorTestCase, RESOURCES_DIR


class TestApiritif(ExecutorTestCase):
    EXECUTOR = ApiritifNoseExecutor
    CMD_LINE = None

    def test_stopping_reason_extracted(self):
        concurrency = 4
        iterations = 3

        self.configure({"execution": {
            "executor": "apiritif",
            "concurrency": concurrency,
            "iterations": iterations,
            "scenario": {
                "script": RESOURCES_DIR + "apiritif/test_stopping_reason.py"
            }
        }})
        self.obj.prepare()
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()
        self.assertEqual(concurrency * iterations, len(self.obj.engine.extracted_stopping_reasons))

