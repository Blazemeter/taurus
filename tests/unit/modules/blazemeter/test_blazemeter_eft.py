"""
Failing integration tests for MOB-43135 (taurus slice) — EFT (Exclude Failed
Transactions) for browser-based tests. Exercises the flag-gated generation hook
inside BlazeMeterUploader.post_process() writing failed_transactions.json to
engine.artifacts_dir.

MOB-43135: EFT (Exclude Failed Transactions) integration tests for browser-based tests.
Exercises the flag-gated generation hook inside BlazeMeterUploader.post_process()
writing failed_transactions.json to engine.artifacts_dir.

Real FunctionalSample / ResultsTree objects are used (not mocked) so the
artifact-generation hook is exercised against the actual aggregation data structure.
Network calls are intercepted via BZMock (the repo's existing convention —
see test_blazeMeterUploader.py::test_no_notes_for_public_reporting).
"""
import json
import os

from bzt.bza import Master, Session
from bzt.modules.functional import FunctionalAggregator, FunctionalSample
from bzt.modules.blazemeter import BlazeMeterUploader
from tests.unit import BZTestCase, EngineEmul, BZMock


def _attach_functional_aggregator(engine):
    """Real FunctionalAggregator wired onto the emulated engine — not mocked."""
    func_agg = FunctionalAggregator()
    engine.aggregator = func_agg
    return func_agg


def _prepare_public_reporting_uploader():
    """Mirrors test_blazeMeterUploader.py::test_no_notes_for_public_reporting's
    hermetic-prepare pattern: BZMock intercepts the connectivity ping, then a
    Session/Master are attached directly so no real HTTP call is made."""
    mock = BZMock()
    obj = BlazeMeterUploader()
    obj.parameters['project'] = 'Proj name'
    obj.settings['token'] = ''  # public reporting — no real BlazeMeter account needed
    obj.settings['browser-open'] = 'none'
    obj.engine = EngineEmul()
    mock.apply(obj._user)
    return obj


class TestArtifactWrittenWhenFlagOn(BZTestCase):
    """Covers T005. Scenario: flag on + >=1 failed transaction -> artifact written."""

    def test_artifact_written_when_flag_on(self):
        obj = _prepare_public_reporting_uploader()
        obj.settings['generate-failed-transactions'] = True

        func_agg = _attach_functional_aggregator(obj.engine)
        failing_sample = FunctionalSample(
            test_case="test_checkout_flow", test_suite="TestCheckout", status="FAILED",
            start_time=1724500000.0, duration=0.842,
            error_msg="Assertion failed: 'Order Confirmed' not found in body",
            error_trace="AssertionError: Assertion failed: 'Order Confirmed' not found in body",
            extras={}, subsamples=[], path=[])
        func_agg.cumulative_results.add_sample(failing_sample)

        obj.prepare()
        obj._session = Session(obj._user, {'id': 1, 'testId': 1, 'userId': 1})
        obj._master = Master(obj._user, {'id': 1})
        obj.send_data = False
        obj.send_monitoring = False

        obj.post_process()

        artifact_path = os.path.join(obj.engine.artifacts_dir, "failed_transactions.json")
        self.assertTrue(
            os.path.exists(artifact_path),
            "failed_transactions.json must be written to engine.artifacts_dir when the "
            "flag is on and there is at least one failed transaction")

        with open(artifact_path) as fh:
            artifact = json.load(fh)
        self.assertEqual(len(artifact["transactions"]), 1)
        self.assertEqual(artifact["transactions"][0]["label"], "test_checkout_flow")


class TestEmptyArtifactWhenNoFailures(BZTestCase):
    """Covers T006. Scenario: only passing transactions -> well-formed empty artifact."""

    def test_empty_artifact_when_no_failures(self):
        obj = _prepare_public_reporting_uploader()
        obj.settings['generate-failed-transactions'] = True

        func_agg = _attach_functional_aggregator(obj.engine)
        passing_sample = FunctionalSample(
            test_case="test_home_page_loads", test_suite="TestHome", status="PASSED",
            start_time=1724500003.0, duration=0.512,
            error_msg=None, error_trace=None, extras={}, subsamples=[], path=[])
        func_agg.cumulative_results.add_sample(passing_sample)

        obj.prepare()
        obj._session = Session(obj._user, {'id': 1, 'testId': 1, 'userId': 1})
        obj._master = Master(obj._user, {'id': 1})
        obj.send_data = False
        obj.send_monitoring = False

        obj.post_process()

        artifact_path = os.path.join(obj.engine.artifacts_dir, "failed_transactions.json")
        self.assertTrue(os.path.exists(artifact_path), "a well-formed artifact must still be written")

        with open(artifact_path) as fh:
            artifact = json.load(fh)
        # US1 scenario 3 — well-formed, but no transaction entries for all-passing runs
        self.assertEqual(artifact["transactions"], [])


class TestArtifactWrittenForGeneralError(BZTestCase):
    """General (non-assertion) error must produce a valid artifact with the error
    in the 'errors' list, not 'assertions'. Regression guard for Counter()
    JSON-serialization bug."""

    def test_artifact_written_for_general_error(self):
        obj = _prepare_public_reporting_uploader()
        obj.settings['generate-failed-transactions'] = True

        func_agg = _attach_functional_aggregator(obj.engine)
        general_error_sample = FunctionalSample(
            test_case="test_login_invalid_creds", test_suite="TestLogin", status="FAILED",
            start_time=1724500001.5, duration=1.203,
            error_msg='no such element: Unable to locate element: {"method":"id","selector":"dashboard-header"}',
            error_trace="selenium.common.exceptions.NoSuchElementException: Message: no such element",
            extras={}, subsamples=[], path=[])
        func_agg.cumulative_results.add_sample(general_error_sample)

        obj.prepare()
        obj._session = Session(obj._user, {'id': 1, 'testId': 1, 'userId': 1})
        obj._master = Master(obj._user, {'id': 1})
        obj.send_data = False
        obj.send_monitoring = False

        obj.post_process()

        artifact_path = os.path.join(obj.engine.artifacts_dir, "failed_transactions.json")
        self.assertTrue(
            os.path.exists(artifact_path),
            "failed_transactions.json must be written for general (non-assertion) errors too")

        with open(artifact_path) as fh:
            artifact = json.load(fh)
        self.assertEqual(len(artifact["transactions"]), 1)
        txn = artifact["transactions"][0]
        self.assertEqual(txn["label"], "test_login_invalid_creds")
        self.assertEqual(len(txn["errors"]), 1, "general error must be in 'errors' list")
        self.assertEqual(len(txn["assertions"]), 0, "general error must NOT be in 'assertions'")


class TestArtifactWithMixedFailures(BZTestCase):
    """Mixed assertion + general error in same run must both appear correctly classified."""

    def test_artifact_with_mixed_failures(self):
        obj = _prepare_public_reporting_uploader()
        obj.settings['generate-failed-transactions'] = True

        func_agg = _attach_functional_aggregator(obj.engine)

        assertion_sample = FunctionalSample(
            test_case="test_checkout_flow", test_suite="TestCheckout", status="FAILED",
            start_time=1724500000.0, duration=0.842,
            error_msg="Assertion failed: 'Order Confirmed' not found in body",
            error_trace="AssertionError: Assertion failed: 'Order Confirmed' not found in body",
            extras={}, subsamples=[], path=[])
        func_agg.cumulative_results.add_sample(assertion_sample)

        general_sample = FunctionalSample(
            test_case="test_login_invalid_creds", test_suite="TestLogin", status="FAILED",
            start_time=1724500001.5, duration=1.203,
            error_msg="no such element",
            error_trace="selenium.common.exceptions.NoSuchElementException: Message: no such element",
            extras={}, subsamples=[], path=[])
        func_agg.cumulative_results.add_sample(general_sample)

        obj.prepare()
        obj._session = Session(obj._user, {'id': 1, 'testId': 1, 'userId': 1})
        obj._master = Master(obj._user, {'id': 1})
        obj.send_data = False
        obj.send_monitoring = False

        obj.post_process()

        artifact_path = os.path.join(obj.engine.artifacts_dir, "failed_transactions.json")
        with open(artifact_path) as fh:
            artifact = json.load(fh)
        self.assertEqual(len(artifact["transactions"]), 2)

        labels = {t["label"] for t in artifact["transactions"]}
        self.assertEqual(labels, {"test_checkout_flow", "test_login_invalid_creds"})


class TestNoArtifactWhenFlagOff(BZTestCase):
    """Covers T013. Scenario: flag absent/false (default) -> no artifact, no error."""

    def test_no_artifact_when_flag_off(self):
        obj = _prepare_public_reporting_uploader()
        # generate-failed-transactions intentionally NOT set — default off (FR-006)

        func_agg = _attach_functional_aggregator(obj.engine)
        failing_sample = FunctionalSample(
            test_case="test_checkout_flow", test_suite="TestCheckout", status="FAILED",
            start_time=1724500000.0, duration=0.842,
            error_msg="Assertion failed", error_trace="AssertionError: Assertion failed",
            extras={}, subsamples=[], path=[])
        func_agg.cumulative_results.add_sample(failing_sample)

        obj.prepare()
        obj._session = Session(obj._user, {'id': 1, 'testId': 1, 'userId': 1})
        obj._master = Master(obj._user, {'id': 1})
        obj.send_data = False
        obj.send_monitoring = False

        obj.post_process()  # must not raise

        artifact_path = os.path.join(obj.engine.artifacts_dir, "failed_transactions.json")
        self.assertFalse(
            os.path.exists(artifact_path),
            "no failed_transactions.json should be produced when the flag is off (default)")
