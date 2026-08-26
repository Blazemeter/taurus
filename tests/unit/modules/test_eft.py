"""
MOB-43135: Unit tests for the bzt.modules.eft helper module (EFT — Exclude Failed
Transactions for browser-based tests).

Exercises:
    classify_failure(label, message, trace, rc, assertion_name) -> error_item_skel dict
    recover_assertion_name(sample) -> str|None
    build_failed_transactions(transactions, session_id) -> artifact dict
"""
import json
import os

from bzt.modules.aggregator import KPISet
from tests.unit import BZTestCase, RESOURCES_DIR


class TestBuildFailedTransactionsShape(BZTestCase):
    """Covers T004. Scenario: shape + schema validation of the EFT artifact."""

    def test_build_failed_transactions_shape(self):
        from bzt.modules.eft import build_failed_transactions

        transactions = [
            {
                "label": "test_checkout_flow",
                "timestamp": 1724500000.0,
                "duration": 0.842,
                "errors": [],
                "assertions": [
                    {"name": "assert::test_checkout_flow", "failureMessage": "boom", "failures": 1}
                ],
                "failedEmbeddedResources": [],
            },
        ]

        artifact = build_failed_transactions(transactions, session_id="sess-123")

        # three-way split keyed by ERRTYPE_* semantics must be present on every transaction
        self.assertIn("reportInfo", artifact)
        self.assertIn("transactions", artifact)
        self.assertEqual(artifact["reportInfo"]["type"], "FAILED_TRANSACTIONS")

        txn = artifact["transactions"][0]
        for key in ("errors", "assertions", "failedEmbeddedResources"):
            self.assertIn(key, txn)
            self.assertIsInstance(txn[key], list)

        # Verify the artifact is JSON-serializable (regression guard for Counter bug)
        json.dumps(artifact)


class TestClassifyGeneralVsAssertion(BZTestCase):
    """Covers T009. Scenario: general error -> ERRTYPE_ERROR, assertion -> ERRTYPE_ASSERT."""

    def test_classify_general_vs_assertion(self):
        from bzt.modules.eft import classify_failure

        # A general (non-assertion) failure — no assertion_name supplied
        general_item = classify_failure(
            label="test_login_invalid_creds",
            message='no such element: Unable to locate element: {"method":"id","selector":"dashboard-header"}',
            trace="selenium.common.exceptions.NoSuchElementException: Message: no such element",
            rc=None,
            assertion_name=None,
        )
        self.assertEqual(general_item["type"], KPISet.ERRTYPE_ERROR)
        self.assertIsNone(general_item["tag"])

        # An assertion failure — assertion_name supplied (recovered or synthetic upstream)
        assertion_item = classify_failure(
            label="test_checkout_flow",
            message="Assertion failed: 'Order Confirmed' not found in body",
            trace="AssertionError: Assertion failed: 'Order Confirmed' not found in body",
            rc=None,
            assertion_name="assert::test_checkout_flow",
        )
        self.assertEqual(assertion_item["type"], KPISet.ERRTYPE_ASSERT)
        self.assertEqual(assertion_item["tag"], "assert::test_checkout_flow")


class TestSyntheticAssertionNameFallback(BZTestCase):
    """
    Covers T010. Scenario: assumption-challenge (A-ASSERT) — assertion name NOT present
    in error_msg/error_trace -> synthetic assert::<label> fallback, still ERRTYPE_ASSERT.

    Uses the REAL fixture (eft_assertion_failure.ldjson) rather than a stub — per
    brownfield-context.md Test Validity Strategy: "DO NOT mock the assertion-name
    extraction being proven".
    """

    def test_synthetic_assertion_name_fallback(self):
        from bzt.modules.functional import FunctionalSample
        from bzt.modules.eft import recover_assertion_name, classify_failure

        # This is the real, checked-in fixture row for the checkout-flow assertion
        # failure (apiritif's assert_in_body raises a bare AssertionError with no
        # separable "name" field — see generator.py::_gen_assertions).
        fixture_path = os.path.join(RESOURCES_DIR, "selenium", "eft_assertion_failure.ldjson")
        with open(fixture_path) as fh:
            rows = [json.loads(line) for line in fh if line.strip()]

        row = next(r for r in rows if r["test_case"] == "test_checkout_flow")
        sample = FunctionalSample(
            test_case=row["test_case"], test_suite=row["test_suite"], status=row["status"],
            start_time=row["start_time"], duration=row["duration"],
            error_msg=row["error_msg"], error_trace=row["error_trace"],
            extras=row.get("extras", {}), subsamples=[], path=row.get("path", []))

        name = recover_assertion_name(sample)

        # The real assertion name is NOT recoverable from this fixture's error_msg/
        # error_trace (no distinguishable name token, just a generic AssertionError).
        # The synthetic fallback derived from the transaction label MUST be produced.
        self.assertEqual(name, "assert::test_checkout_flow")

        item = classify_failure(
            label=sample.get_short_name(), message=sample.error_msg, trace=sample.error_trace,
            rc=None, assertion_name=name)
        self.assertEqual(item["type"], KPISet.ERRTYPE_ASSERT)
        self.assertTrue(item["tag"])  # never empty (FR-004 / SC-003)


class TestEmptyResponseBodiesAndNullRc(BZTestCase):
    """Covers T015. Scenario: graceful degradation for browser tests (FR-005)."""

    def test_empty_response_bodies_and_null_rc(self):
        from bzt.modules.eft import classify_failure

        item = classify_failure(
            label="test_login_invalid_creds",
            message="no such element",
            trace="selenium.common.exceptions.NoSuchElementException",
            rc=None,
            assertion_name=None,
        )
        self.assertIsNone(item["rc"])
        self.assertEqual(item["responseBodies"], [])
