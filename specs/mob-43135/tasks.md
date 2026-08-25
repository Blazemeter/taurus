---
description: "Task list for EFT (Exclude Failed Transactions) — browser-based tests, Taurus side"
---

# Tasks: Exclude Failed Transactions (EFT) for Browser-Based Tests — Taurus Side (MOB-43135)

**Input**: Design documents from `specs/mob-43135/`
**Prerequisites**: plan.md, spec.md, data-model.md, contracts/failed_transactions.schema.json, brownfield-context.md

**Tests**: Included and REQUIRED (constitution III — Test-First NON-NEGOTIABLE). Test tasks precede implementation tasks within each story.

**Binding constraints** (from brownfield-context.md — DO NOT violate):
- DO NOT modify `JTLErrorsReader` / `bzt/modules/jmeter.py` for non-JMeter formats — extract classification only.
- DO NOT gate EFT by executor type or account flag inside taurus — gating is server-side (a.blazemeter.com).
- All contract verification is source/static + local fixtures — NO dev/stage/prod/live endpoint calls.

## Phase 1: Setup (Shared Infrastructure)

- [ ] T001 Create the realistic Selenium/Apiritif assertion-failure fixture at `tests/resources/selenium/eft_assertion_failure.ldjson` containing at least one failed assertion transaction and one general-error transaction (used by the assertion-name probe and downstream tests). Supports AC "Selenium error categorization".

## Phase 2: Foundational (blocking prerequisites for all user stories)

- [ ] T002 [P] Contract-verification / POC (A-ASSERT): write a probe test at `tests/unit/modules/test_eft_assertion_probe.py` that loads `tests/resources/selenium/eft_assertion_failure.ldjson` into `FunctionalSample` objects and asserts whether the assertion name is recoverable from `FunctionalSample.error_msg`/`error_trace`. Document the outcome; if the real name is NOT recoverable, the probe MUST assert that a synthetic name derived from the transaction label is produced instead. Resolves the A-ASSERT VALIDATE assumption before implementation. Supports AC "Selenium error categorization" (FR-004).
- [ ] T003 Scaffold the new helper module `bzt/modules/eft.py` with the public surface used by the uploader: `classify_failure(label, message, trace, rc, assertion_name) -> error_item_skel dict`, `recover_assertion_name(sample) -> str|None`, and `build_failed_transactions(transactions, session_id) -> artifact dict`. Signatures + docstrings referencing the error_item_skel shape (aggregator.py:271-279) and the three-way split (blazemeter_reporter.py:695-717). Supports all ACs.

## Phase 3: User Story 1 — Generate a generic EFT json artifact for Selenium runs (Priority: P1)

**Goal**: Produce `failed_transactions.json` in `engine.artifacts_dir` at post_process when the flag is on. (FR-001, FR-002, SC-001)

**Independent test**: Run a Selenium fixture through the engine with the flag on; assert the artifact is written and validates against `contracts/failed_transactions.schema.json`.

- [ ] T004 [US1] Write failing test `tests/unit/modules/test_eft.py::test_build_failed_transactions_shape` asserting `build_failed_transactions` emits the three-way split (`errors`/`assertions`/`failedEmbeddedResources`) keyed by ERRTYPE_* and validates against `contracts/failed_transactions.schema.json`. Covers AC "Taurus generates generic EFT json artifact".
- [ ] T005 [US1] Write failing integration test `tests/unit/modules/blazemeter/test_blazemeter_eft.py::test_artifact_written_when_flag_on` (BZTestCase + EngineEmul) asserting `failed_transactions.json` is written to `engine.artifacts_dir` after post_process with the flag on and ≥1 failed transaction. Covers AC "Taurus generates generic EFT json artifact".
- [ ] T006 [US1] Write failing test `tests/unit/modules/blazemeter/test_blazemeter_eft.py::test_empty_artifact_when_no_failures` asserting a well-formed artifact with empty collections when the run has only passing transactions. Covers AC "Taurus generates generic EFT json artifact".
- [ ] T007 [US1] Implement `build_failed_transactions` in `bzt/modules/eft.py` — assemble `reportInfo` + `transactions[]` mirroring `DatapointSerializer.__add_errors` split (blazemeter_reporter.py:695-717); make the shape test pass. Implements AC "Taurus generates generic EFT json artifact" (FR-002).
- [ ] T008 [US1] Implement the flag-gated generation hook in `BlazeMeterUploader.post_process()` (`bzt/modules/blazemeter/blazemeter_reporter.py`): read `FunctionalAggregator.cumulative_results`, build the artifact, write to `os.path.join(engine.artifacts_dir, "failed_transactions.json")`; rely on the existing `__upload_artifacts` (blazemeter_reporter.py:268-298) for upload; make the integration tests pass. Implements AC "Taurus generates generic EFT json artifact" (FR-001).

## Phase 4: User Story 2 — Categorize Selenium errors into general errors vs. failed assertions (Priority: P1)

**Goal**: Classify each failure via the existing ERRTYPE_* enums; recover or synthesize assertion names. (FR-003, FR-004, SC-003)

**Independent test**: Feed a fixture with a general error and a failed assertion; assert correct bucketing and a non-empty assertion name.

- [ ] T009 [US2] Write failing test `tests/unit/modules/test_eft.py::test_classify_general_vs_assertion` asserting a non-assertion failure → `errors` (ERRTYPE_ERROR) and an assertion failure → `assertions` (ERRTYPE_ASSERT). Covers AC "Selenium error categorization".
- [ ] T010 [US2] Write failing assumption-challenge test `tests/unit/modules/test_eft.py::test_synthetic_assertion_name_fallback` where the assertion name is NOT present in `error_msg`/`error_trace`, asserting a synthetic `assert::<label>` name is produced and the item stays ERRTYPE_ASSERT. Do NOT mock the extraction being proven — use the real fixture path. Covers AC "Graceful degradation" (FR-004).
- [ ] T011 [US2] Implement `classify_failure` in `bzt/modules/eft.py` by EXTRACTING (not importing-and-mutating) the ERRTYPE_* classification logic from `JTLErrorsReader.find_failure` (jmeter.py:1413-1454); `bzt/modules/jmeter.py` stays UNCHANGED; make the categorization test pass. Implements AC "Selenium error categorization" (FR-003, FR-008).
- [ ] T012 [US2] Implement `recover_assertion_name` + synthetic fallback in `bzt/modules/eft.py`; make the synthetic-name test pass. Implements AC "Graceful degradation" (FR-004).

## Phase 5: User Story 3 — Opt-in configuration, default off (Priority: P2)

**Goal**: Generation fires only when `modules.blazemeter.generate-failed-transactions` is truthy; default off = zero behaviour change. (FR-006, SC-002)

**Independent test**: Load config with and without the flag; assert generation fires only when set.

- [ ] T013 [US3] Write failing test `tests/unit/modules/blazemeter/test_blazemeter_eft.py::test_no_artifact_when_flag_off` asserting no artifact is produced and no error raised when the flag is absent/false (default). Covers AC "Opt-in config flag default off".
- [ ] T014 [US3] Ensure the config read `self.settings.get("generate-failed-transactions", False)` guards the whole generation path in `BlazeMeterUploader.post_process()` (`bzt/modules/blazemeter/blazemeter_reporter.py`); make the flag-off test pass. Implements AC "Opt-in config flag default off" (FR-006).

## Phase 6: Graceful degradation (FR-004, FR-005)

- [ ] T015 [P] Write failing test `tests/unit/modules/test_eft.py::test_empty_response_bodies_and_null_rc` asserting browser-test error items carry empty `responseBodies` and tolerate null `rc`. Covers AC "Graceful degradation".
- [ ] T016 Implement graceful-degradation handling in `bzt/modules/eft.py` (empty `responseBodies`, null `rc` tolerated); make the degradation test pass. Implements AC "Graceful degradation" (FR-005).

## Phase 7: Polish & Cross-Cutting Concerns

- [ ] T017 [P] Add YAML config docs for `modules.blazemeter.generate-failed-transactions` to the repo docs location (`site/dat/docs/`) with a browser-EFT example matching `specs/mob-43135/quickstart.md`. Documentation task.
- [ ] T018 Dependency / coordination (A-SPARTA): request and record Sparta (Taurus team) sign-off on the initial `failed_transactions.json` schema (`contracts/failed_transactions.schema.json`). The initial format is used now; capture any required schema changes as a follow-up. NON-BLOCKING for this repo's implementation per task-reviewer handoff. No code change.
- [ ] T019 Run the full `nose2` unit suite (`python -m nose2 -s tests/unit -v`) and confirm it is green — verifies FR-007/FR-008 (no JTLErrorsReader change, no taurus-side gating) via non-regression. Quality gate — N/A — nose2 suite.

## Dependencies & Execution Order

- Setup phase precedes the Foundational phase, which precedes all user-story phases.
- User Story 1 is the MVP; User Story 2 and User Story 3 build on the Foundational scaffold and the User-Story-1 uploader hook.
- Within each story: test tasks precede implementation tasks (constitution III — Test-First).
- The classification-extraction task must NOT modify `jmeter.py`; the uploader hook and the config-guard are the only edits to `blazemeter_reporter.py`.
- The Polish phase runs last. The Sparta coordination task is non-blocking; the green-suite run is the final gate.

## Parallel opportunities
- The assertion-name probe (Foundational, marked [P]) can run alongside the helper scaffold — different files.
- Test-authoring rows marked [P] touch independent sections of `test_eft.py`; coordinate to avoid file-edit races if run truly concurrently.
- The documentation task (Polish, marked [P]) can run alongside code polish.

## MVP scope
User Story 1 delivers a working, uploadable `failed_transactions.json` for Selenium runs — the minimum viable increment.
