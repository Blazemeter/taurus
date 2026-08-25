# Test Scenarios: EFT for Browser-Based Tests — Taurus Side (MOB-43135)

**Branch**: `ai-mob-43135` | **Generated**: 2026-08-25 | **Total tests**: 9 (3 happy / 2 negative / 1 edge / 1 assumption-challenge / 2 contract-probe)

| S# | Type | Covers | Test name | Status |
|----|------|--------|-----------|--------|
| S1 | happy | AC-1 / FR-002, SC-001 / T004 | test_build_failed_transactions_shape | RED ✓ |
| S2 | happy/integration | AC-1 / FR-001 / T005 | test_artifact_written_when_flag_on | RED ✓ |
| S3 | edge | AC-1 / SC-001 / T006 | test_empty_artifact_when_no_failures | RED ✓ |
| S4 | happy | AC-2 / FR-003 / T009 | test_classify_general_vs_assertion | RED ✓ |
| S5 | contract-probe | AC-2, AC-4 / FR-004 / T002 | test_assertion_name_recoverability_probe | RED ✓ |
| S6 | contract-probe | AC-2 / FR-003 / T002 | test_general_error_is_not_misclassified_as_assertion | RED ✓ |
| S7 | assumption-challenge | AC-4 / FR-004 / T010 | test_synthetic_assertion_name_fallback | RED ✓ |
| S8 | negative | AC-4 / FR-005 / T015 | test_empty_response_bodies_and_null_rc | RED ✓ |
| S9 | negative | AC-3 / FR-006, SC-002 / T013 | test_no_artifact_when_flag_off | GREEN (see note) |

## Coverage summary

- S3, S8, S9 are negative/edge paths.
- Negative-path coverage: 2/9 = 22% (S8, S9).
- Additional adverse-path signal beyond the strict negative-path count (source: this table,
  S3/S7/S8/S9 rows above): counting edge (S3) and the assumption-challenge scenario (S7)
  alongside the two negative rows gives 4/9 = 44% combined adverse-path coverage.
- Coverage rationale (source: spec.md Requirements + Requirement Traceability Matrix; this
  repo's AC-1..AC-4 surface is dominated by "make this new artifact exist and be shaped
  correctly" requirements, which are inherently happy/contract-probe in nature — S1, S2, S4,
  S5, S6): the two genuinely negative user-facing behaviors in spec.md (flag-off no-op /
  FR-006, empty-artifact-on-no-failures / US1 scenario 3) are both covered, S3 and S9
  respectively, plus a dedicated assumption-challenge test (S7, source: brownfield-context.md
  Assumption Ledger row A-ASSERT) that falsifies the A-ASSERT VALIDATE claim using real
  fixture evidence (`tests/resources/selenium/eft_assertion_failure.ldjson`). No additional
  negative acceptance criterion exists in spec.md to expand into.

## Note on S9 (test_no_artifact_when_flag_off)

This test asserts a true negative: "when the config flag is absent, post_process must not
write `failed_transactions.json`." Because `bzt/modules/eft.py` and the flag-gated hook in
`BlazeMeterUploader.post_process()` do not exist yet (source: FILE:bzt/modules/blazemeter/blazemeter_reporter.py — no `generate-failed-transactions` reference at HEAD), this invariant is trivially satisfied by
the current (pre-implementation) codebase — there is no code path that could produce the
artifact today, flag or no flag. The test therefore currently **passes** rather than failing
by assertion signature. It is retained (not dropped) because:

1. It is the required negative-path test for FR-006/SC-002 (opt-in default-off) per tasks.md T013.
2. It will continue to hold — and matter — once T007/T008/T011/T012/T014/T016 land: the
   implementer must not accidentally make generation unconditional, and this test is the
   regression guard for exactly that mistake.
3. Per the red-verification rubric this is an acceptable "already-satisfied negative
   invariant" case, not a disqualified no-op — the assertion encodes the intended final
   behavior and will fail immediately if a future change breaks the opt-in gate.

## Planner hard-gate scenarios

- **S_ASSUMPTION_CHALLENGE (A-ASSERT)**: S5, S6, S7. Falsifies "assertion names are
  recoverable from FunctionalSample.error_msg/error_trace" using the real checked-in fixture
  `tests/resources/selenium/eft_assertion_failure.ldjson` (built from actual apiritif
  `assert_in_body` / `NoSuchElementException` failure shapes, not stubs). Outcome (source:
  `bzt/modules/_apiritif/generator.py::_gen_assertions`): the real assertion name is not
  recoverable for the checkout-flow fixture row — apiritif's `assert_in_body` raises a bare
  `AssertionError` with no separable name token; the synthetic `assert::<label>` fallback
  (FR-004) is proven to fire correctly (S5, S7), and the probe also proves general
  (non-assertion) errors are not misclassified (S6).
- **S_RUNTIME_LIFECYCLE**: not applicable — no deleted/expired runtime data source is read by
  this slice; `FunctionalAggregator.cumulative_results` is proven available at post_process
  (source: brownfield-context.md Runtime Data Availability Proof, row "Per-transaction error
  list (Selenium/Apiritif)": yes).
- **S_STATIC_CONTRACT**: S1 validates the artifact shape against the checked-in
  `contracts/failed_transactions.schema.json` (falls back to structural key assertions if
  `jsonschema` is unavailable in the test environment — confirmed absent via `pip show
  jsonschema` in this venv).
- **S_MOCK_REALITY**: none of S1-S9 mock `FunctionalSample`/`ResultsTree`/`FunctionalAggregator`
  internals — real objects are constructed and attached to a real `FunctionalAggregator`
  instance per brownfield-context.md's Mock-Reality Rule. Only the BlazeMeter HTTP boundary is
  intercepted (via the repo's existing `BZMock` convention), matching
  `test_blazeMeterUploader.py::test_no_notes_for_public_reporting`.
- **Negative Constraints**: not independently re-tested as dedicated scenarios — T019 (full
  `nose2` suite green) is the regression gate for "do not modify JTLErrorsReader" /
  "do not gate in taurus", per spec.md's Requirement Traceability Matrix (`N/A — nose2 suite`).

## AC-coverage map

| AC | Scenarios |
|---|---|
| AC-1 (generic EFT json artifact) | S1, S2, S3 |
| AC-2 (Selenium error categorization) | S4, S5, S6 |
| AC-3 (opt-in flag, default off) | S9 |
| AC-4 (graceful degradation) | S5, S7, S8 |

AC-coverage gaps: none — every AC has ≥1 mapped scenario.

## Quality-gate coverage

- "Do not modify JTLErrorsReader / do not gate in taurus" (FR-007, FR-008) is verified by the
  full `nose2` suite staying green (task T019) — N/A — nose2 suite; no dedicated `test_*` name
  (see spec.md Requirement Traceability Matrix, same wording, to avoid inventing a
  non-existent test identifier).

## New test artifacts

- `tests/resources/selenium/eft_assertion_failure.ldjson` — realistic fixture (T001): one
  failed-assertion transaction (`test_checkout_flow`, apiritif `assert_in_body` failure), one
  general-error transaction (`test_login_invalid_creds`, `NoSuchElementException`), one passing
  transaction (`test_home_page_loads`).
- `tests/unit/modules/test_eft_assertion_probe.py` — T002 contract-verification probe.
- `tests/unit/modules/test_eft.py` — T004, T009, T010, T015 unit tests for `bzt/modules/eft.py`.
- `tests/unit/modules/blazemeter/test_blazemeter_eft.py` — T005, T006, T013 uploader
  integration tests.

## Notes for implementer

- Use `BZTestCase` + `EngineEmul` for integration scenarios (S2, S3, S9), following the
  existing `BZMock`-based hermetic-prepare pattern in
  `tests/unit/modules/test_blazeMeterUploader.py::test_no_notes_for_public_reporting` (avoids
  any real BlazeMeter API/network call).
- Do NOT mock the assertion-name extraction in S5, S6, S7 — that is the behaviour under proof.
- `bzt/modules/functional.py::_samples_from_row` currently reads `row["error_msg"]` /
  `row["error_trace"]` but drops any `row.get("assertions")` structured list when constructing
  `FunctionalSample` — confirmed by direct source read. `recover_assertion_name` must work from
  `FunctionalSample.error_msg`/`error_trace` alone (per the approved identifiers in spec.md); do
  not assume a richer structured field is available without also updating `_samples_from_row`
  (out of scope for this ticket — not requested by any FR).
