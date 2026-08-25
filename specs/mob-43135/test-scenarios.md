# Test Scenarios: EFT for Browser-Based Tests — Taurus Side (MOB-43135)

> Authored by task-planner as the initial scenario map; task-test-author expands these into
> full failing tests (red-verified) under `tests/unit/`. Test names here match the
> Requirement Traceability Matrix in spec.md/plan.md and the T### tasks in tasks.md.

## Scenario table

| # | Test name | Category | Covers (AC / FR) | Task | Given | When | Then | Must-not-mock |
|---|---|---|---|---|---|---|---|---|
| 1 | test_build_failed_transactions_shape | positive | Taurus generates EFT json / FR-002 | T004 | a set of typed failure items | build_failed_transactions is called | output has errors/assertions/failedEmbeddedResources split keyed by ERRTYPE_* and validates against contracts/failed_transactions.schema.json | the schema validation |
| 2 | test_artifact_written_when_flag_on | positive/integration | Taurus generates EFT json / FR-001 | T005 | a Selenium run (EngineEmul) with the flag on and ≥1 failed transaction | post_process runs | failed_transactions.json exists in engine.artifacts_dir | the artifacts_dir write |
| 3 | test_empty_artifact_when_no_failures | edge | Taurus generates EFT json / SC-001 | T006 | a Selenium run with only passing transactions, flag on | post_process runs | a well-formed artifact with empty collections is written | — |
| 4 | test_classify_general_vs_assertion | positive | Selenium error categorization / FR-003 | T009 | a fixture with one general error and one failed assertion | classify_failure runs | general → errors (ERRTYPE_ERROR), assertion → assertions (ERRTYPE_ASSERT) | the ERRTYPE classification |
| 5 | test_eft_assertion_probe | contract/POC | Selenium error categorization / FR-004 | T002 | the real eft_assertion_failure.ldjson fixture | assertion-name recovery is attempted | either the real assertion name is recovered OR a synthetic name is produced (documented outcome) | the assertion-name extraction being proven |
| 6 | test_synthetic_assertion_name_fallback | negative/assumption-challenge | Graceful degradation / FR-004 | T010 | an assertion failure whose name is NOT present in error_msg/error_trace | classify runs | a synthetic assert::<label> name is produced, item stays ERRTYPE_ASSERT | the name-absence condition (use real fixture, not a stub name) |
| 7 | test_no_artifact_when_flag_off | negative | Opt-in config flag / FR-006 | T013 | a config without the flag (default) | post_process runs | no artifact produced, no error raised | — |
| 8 | test_empty_response_bodies_and_null_rc | edge | Graceful degradation / FR-005 | T015 | a browser-test failure with no response body and null rc | classify runs | error item has empty responseBodies and tolerates null rc | — |

## Negative / edge coverage
- Scenarios 3, 6, 7, 8 are negative/edge paths (≥30% of the map), covering: no-failures artifact, missing-assertion-name fallback, flag-off no-op, and browser-degradation.

## Quality-gate coverage
- "Do not modify JTLErrorsReader / do not gate in taurus" (FR-007, FR-008) is verified by the full `nose2` suite staying green (task T019) — N/A — nose2 suite; no dedicated `test_*` name.

## Notes for test-author
- Use `BZTestCase` + `EngineEmul` for integration scenarios (2, 3, 7).
- Do NOT mock the assertion-name extraction in scenarios 5 and 6 — that is the behaviour under proof.
- Red-verify each test by its assertion-failure signature before implementation lands.
