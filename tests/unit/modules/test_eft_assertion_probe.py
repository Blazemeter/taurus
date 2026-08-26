"""
MOB-43135: Contract probe for assertion-name recoverability in EFT.

Claim under test: "Assertion names for Selenium/Apiritif failures can be recovered
from FunctionalSample.error_msg/error_trace."

Loads the REAL checked-in fixture (tests/resources/selenium/eft_assertion_failure.ldjson)
into FunctionalSample objects and proves whether the assertion name is recoverable.
If the real name is NOT recoverable, asserts that a synthetic name derived from the
transaction label is produced instead. Not mocked — the point is to prove/disprove
the assumption against real data shapes.
"""
import json
import os

from bzt.modules.functional import FunctionalSample
from tests.unit import BZTestCase, RESOURCES_DIR


class TestEftAssertionProbe(BZTestCase):
    def _load_fixture(self):
        fixture_path = os.path.join(RESOURCES_DIR, "selenium", "eft_assertion_failure.ldjson")
        with open(fixture_path) as fh:
            return [json.loads(line) for line in fh if line.strip()]

    def test_assertion_name_recoverability_probe(self):
        from bzt.modules.eft import recover_assertion_name

        rows = self._load_fixture()
        failed_rows = [r for r in rows if r["status"] == "FAILED"]
        self.assertGreaterEqual(len(failed_rows), 1, "fixture must contain at least one failed transaction")

        checkout_row = next(r for r in failed_rows if r["test_case"] == "test_checkout_flow")
        sample = FunctionalSample(
            test_case=checkout_row["test_case"], test_suite=checkout_row["test_suite"],
            status=checkout_row["status"], start_time=checkout_row["start_time"],
            duration=checkout_row["duration"], error_msg=checkout_row["error_msg"],
            error_trace=checkout_row["error_trace"], extras=checkout_row.get("extras", {}),
            subsamples=[], path=checkout_row.get("path", []))

        recovered = recover_assertion_name(sample)

        # Documented outcome: apiritif's assert_in_body raises a bare AssertionError
        # with no separable "name" token in error_msg or error_trace (confirmed via
        # generator.py::_gen_assertions — assert_in_body/assert_status_code calls
        # carry no name argument). The real name is therefore NOT recoverable here,
        # so the synthetic fallback MUST fire.
        self.assertEqual(
            recovered, "assert::test_checkout_flow",
            "A-ASSERT is NOT provable from error_msg/error_trace for this real fixture shape; "
            "synthetic fallback (derived from the transaction label) must be used per FR-004")

    def test_general_error_is_not_misclassified_as_assertion(self):
        from bzt.modules.eft import recover_assertion_name

        rows = self._load_fixture()
        login_row = next(r for r in rows if r["test_case"] == "test_login_invalid_creds")
        sample = FunctionalSample(
            test_case=login_row["test_case"], test_suite=login_row["test_suite"],
            status=login_row["status"], start_time=login_row["start_time"],
            duration=login_row["duration"], error_msg=login_row["error_msg"],
            error_trace=login_row["error_trace"], extras=login_row.get("extras", {}),
            subsamples=[], path=login_row.get("path", []))

        # NoSuchElementException is a general/browser error, not an assertion failure.
        # recover_assertion_name must not fabricate an assertion name for it — the
        # caller (classify_failure) is responsible for deciding ERRTYPE based on
        # whether an assertion was actually raised, not based on recover_assertion_name
        # returning a non-None value for every failure.
        recovered = recover_assertion_name(sample)
        self.assertIsNone(
            recovered,
            "recover_assertion_name must return None for non-assertion (general error) samples")
