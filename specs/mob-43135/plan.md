# Implementation Plan: Exclude Failed Transactions (EFT) for Browser-Based Tests — Taurus Side

**Branch**: `ai-mob-43135` | **Date**: 2026-08-25 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `specs/mob-43135/spec.md`; brownfield research from `specs/mob-43135/brownfield-context.md`

## Summary

Taurus must produce a generic EFT-compatible json artifact (`failed_transactions.json`) for browser-based (Selenium/Apiritif) runs, categorizing failures into general errors vs. failed assertions using the existing ERRTYPE_* classification, gated by a new opt-in config flag `modules.blazemeter.generate-failed-transactions` (default off). The artifact is written to `engine.artifacts_dir` and uploaded via the existing `__upload_artifacts` path.

**Technical approach** (from brownfield Design Alternatives, SELECTED):
- **Extract classification (Option A)**: factor the ERRTYPE_* classification currently inlined in `JTLErrorsReader.find_failure` (`bzt/modules/jmeter.py:1413-1454`) into a shared, executor-agnostic helper. Do **not** repurpose `JTLErrorsReader` for non-JMeter formats; a new Selenium error path uses the shared helper.
- **Generate in the uploader (Option C)**: build and write `failed_transactions.json` inside `BlazeMeterUploader.post_process()` (`bzt/modules/blazemeter/blazemeter_reporter.py:300-349`), reading `engine.aggregator` (KPI) and the `FunctionalAggregator.cumulative_results` (`ResultsTree`) for functional transactions. Guard the whole path behind the new config flag.
- **Mirror the proven shape**: the artifact reuses the `error_item_skel` dict keys and the `DatapointSerializer.__add_errors` three-way split (`errors` / `assertions` / `failedEmbeddedResources`).

## Technical Context

**Language/Version**: Python 3 (per `setup.py`; must run on all supported interpreters the nose2 CI matrix targets)
**Primary Dependencies**: Taurus/`bzt` framework internals only — `bzt.modules.aggregator` (KPISet, error_item_skel), `bzt.modules.functional` (FunctionalSample/ResultsTree/FunctionalAggregator), `bzt.modules.blazemeter.blazemeter_reporter` (BlazeMeterUploader, DatapointSerializer). Standard-library `json` for serialization. No new third-party deps.
**Storage**: Local file in `engine.artifacts_dir` (`failed_transactions.json`), bundled into `artifacts.zip`. No database.
**Testing**: `nose2` unit tests under `tests/unit/` against `BZTestCase` + `EngineEmul` (`python -m nose2 -s tests/unit -v`).
**Target Platform**: Cross-platform (Linux/macOS/Windows) wherever `bzt` runs; the artifact path is filesystem-portable via `engine.artifacts_dir`.
**Project Type**: CLI / test-automation library (single project).
**Performance Goals**: Negligible — a single in-memory pass over `cumulative_results` at post_process. No per-transaction network calls (see Performance note).
**Constraints**: Must not change default behaviour when flag is off; must not modify `JTLErrorsReader`; must not gate by executor type in taurus; no import-time or `__init__`-time I/O (constitution IV).
**Scale/Scope**: Bounded by the number of transactions in a single test run (typically 10s–1000s), all already resident in memory.

## Performance note (N×M gate — not applicable)

This feature performs a **single in-memory pass** over `FunctionalAggregator.cumulative_results` and the KPI error lists at post_process, then writes one JSON file. There are **no nested external API calls** (no outer-loop-spawns-inner-API-call pattern), so the N×M External-Call Performance Gate (PERF-001..008) does not apply. The only cost is JSON serialization of already-aggregated data; it is O(transactions × errors-per-transaction) CPU with no network fan-out. Degradation contract: if the functional aggregator holds no results (e.g. a pure-JMeter run with the flag on), the generator writes a well-formed artifact with empty collections rather than raising.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-checked after Phase 1 design.*

| Principle | Verdict | Reasoning |
|---|---|---|
| I. YAML-First Unified Interface | **PASS** | New behaviour is reachable via a stable YAML key `modules.blazemeter.generate-failed-transactions`; no hidden entry point. |
| II. Wrapper Fidelity Over Reinvention | **PASS** | Reuses existing ERRTYPE_* classification and the existing artifact-upload path; extracts (not reinvents) the classification helper. No re-implementation of executor engines. |
| III. Test-First (NON-NEGOTIABLE) | **PASS** | All new code lands with `nose2` unit tests under `tests/unit/` against `BZTestCase`/`EngineEmul`, including a Selenium assertion-failure fixture; the suite must stay green. Enforced by tasks.md ordering (tests authored by task-test-author before implementation). |
| IV. Module Lifecycle Discipline | **PASS** | Generation runs in `post_process()` only; no import-time or `__init__` I/O; the config flag is read during the normal lifecycle. |
| V. Observable, Aggregatable Results | **PASS** | The generator consumes aggregated `FunctionalSample`/`DataPoint` data via the aggregator; it does not reach into executor internals. Errors/warnings logged through the module logger. |

No violations → Complexity Tracking is empty.

## Approach (detailed)

### 1. Config flag (FR-006, US3)
- Add `generate-failed-transactions` (bool, default `False`) to the BlazeMeter uploader's settings read in `post_process()` of `BlazeMeterUploader` (`bzt/modules/blazemeter/blazemeter_reporter.py`). Read via the module's `self.settings.get("generate-failed-transactions", False)`.
- When falsy: skip all generation — zero behaviour change.

### 2. Shared error classification helper (FR-003, FR-008, Option A)
- Create a new module-level helper (`bzt/modules/eft.py`) that maps a failure record to an ERRTYPE_* value and produces an `error_item_skel`-shaped dict.
- Source the classification logic from `JTLErrorsReader.find_failure` (`bzt/modules/jmeter.py:1413-1454`) — **extract**, do not import-and-mutate the JMeter reader.
- The helper accepts a normalized failure input (label, message, rc, trace, assertion-name-if-known) and returns the `error_item_skel` dict.

### 3. Selenium error extraction (FR-003, FR-004)
- Read `FunctionalAggregator.cumulative_results` (`ResultsTree`) at post_process; for each `FunctionalSample` with status FAILED/BROKEN, extract `label`, `error_msg`, `error_trace`, timestamps.
- Classify assertion vs. general error via the shared helper. Assertion detection heuristic mirrors JMeter: an assertion failure is distinguished by the presence of an assertion marker in the error message/trace (the exact recovery of the assertion *name* is a VALIDATE assumption — see A-ASSERT).
- **Assertion-name fallback (FR-004)**: if the assertion name cannot be recovered, synthesize one from the transaction label (e.g. `assert::<label>`), still typed ERRTYPE_ASSERT.

### 4. Artifact assembly & write (FR-001, FR-002)
- Build a per-run structure: `reportInfo` (session/test/timestamp/type=FAILED_TRANSACTIONS) + `transactions[]`, each with `label`, `timestamp`, `duration`, and the three-way error split (`errors` / `assertions` / `failedEmbeddedResources`) keyed by ERRTYPE_*, mirroring `DatapointSerializer.__add_errors` (`bzt/modules/blazemeter/blazemeter_reporter.py:695-717`).
- Serialize with stdlib `json`; write to `os.path.join(engine.artifacts_dir, "failed_transactions.json")`.
- The existing `__upload_artifacts()` (`bzt/modules/blazemeter/blazemeter_reporter.py:268-298`) picks it up automatically — **no new upload logic** (FR-001).

### 5. Graceful degradation (FR-005)
- `responseBodies` → empty list for browser tests (no JMeter-style bodies).
- `rc` → `None` tolerated.
- No failed transactions → well-formed artifact with empty collections.

### Runnable pseudocode (artifact generation, post_process)

```python
# In BlazeMeterUploader.post_process (bzt/modules/blazemeter/blazemeter_reporter.py)
def post_process(self):
    super().post_process()          # existing behaviour (upload KPI, artifacts, etc.)
    if not self.settings.get("generate-failed-transactions", False):
        return                      # FR-006: opt-in, default off -> zero behaviour change

    from bzt.modules.eft import build_failed_transactions, classify_failure, recover_assertion_name
    transactions = []
    func_agg = self._find_functional_aggregator()   # engine.aggregator or a functional aggregator
    results_tree = func_agg.cumulative_results if func_agg else None   # ResultsTree, in-memory (proven available at post_process)

    if results_tree is not None:
        for label, samples in results_tree.items():          # single in-memory pass; no network I/O
            for sample in samples:                            # FunctionalSample
                if sample.status not in ("FAILED", "BROKEN"):
                    continue
                # classify_failure() is the shared helper extracted from JTLErrorsReader.find_failure
                item = classify_failure(
                    label=label,
                    message=sample.error_msg,                 # may be None
                    trace=sample.error_trace,                 # may be None
                    rc=None,                                  # FR-005: rc tolerated as None for browser tests
                    assertion_name=recover_assertion_name(sample) or ("assert::%s" % label),  # FR-004 synthetic fallback
                )
                # item is error_item_skel-shaped: {cnt,msg,tag,rc,type,urls,responseBodies}
                item["responseBodies"] = []                   # FR-005: empty for browser tests
                transactions.append({
                    "label": label,
                    "timestamp": getattr(sample, "start_time", None),
                    "duration": getattr(sample, "duration", None),
                    "item": item,
                })

    # split by ERRTYPE_* mirroring DatapointSerializer.__add_errors
    artifact = build_failed_transactions(transactions, session_id=self._session_id())
    out_path = os.path.join(self.engine.artifacts_dir, "failed_transactions.json")
    with open(out_path, "w") as fh:
        json.dump(artifact, fh)      # existing __upload_artifacts bundles this into artifacts.zip
```

(Field names/paths above are anchored to the Dependency Contract / error_item_skel proven in brownfield-context.md. `recover_assertion_name` is the A-ASSERT VALIDATE point — its real behaviour is proven by the POC/contract task before implementation, with the synthetic fallback guaranteed regardless.)

## Assumption Ledger

| ID | Assumption / Claim | Type | Evidence For | Evidence Against / Unknowns | Risk | Validation Required | Decision |
|---|---|---|---|---|---|---|---|
| A-ASSERT | Assertion names for Selenium/Apiritif failures are recoverable from `FunctionalSample.error_msg`/`error_trace` | shape/availability | jmeter.py:1413-1454 recovers assert name from XML tag; functional.py:67-110 exposes error_msg/error_trace | Apiritif/Selenium may embed the name only in a traceback string, or not at all | medium | POC/contract task reading a real assertion-failure fixture; prove recoverability or trigger synthetic-name fallback | **VALIDATE** |
| A-SPARTA | Initial json format mirroring jmx EFT is acceptable to Sparta as a starting point | dependency | task-reviewer handoff records Sparta approval as non-blocking | Sparta may require a different top-level shape | medium | Sparta sign-off task before final format lock | **VALIDATE** |
| A-UPLOAD | Writing to `engine.artifacts_dir` results in upload via `__upload_artifacts` | integration | blazemeter_reporter.py:268-298 zips everything in artifacts_dir | none found | low | covered by uploader integration test | **PROVEN** |
| A-RUNTIME | Functional transaction/error data is available in `cumulative_results` at post_process | availability | functional.py:47-128 (process_readers in check + post_process) | none found | low | covered by generator test with functional fixture | **PROVEN** |

No high/critical assumption is left as ASSUME/PROCEED.

## Boundary Compatibility Analysis

| Boundary | Producer / Source Shape | Consumer / Expected Shape | Compatibility Proof | Adapter / Mapping | Decision |
|---|---|---|---|---|---|
| FunctionalSample → error item | `FunctionalSample` (label, status, error_msg, error_trace, timestamps) — functional.py:67-110 | `error_item_skel` dict (cnt,msg,tag,rc,type,urls,responseBodies) — aggregator.py:271-279 | shapes differ; a mapping is required | `classify_failure` helper maps sample → error_item_skel | USE WITH ADAPTER |
| error items → artifact json | list of typed error items | three-way split errors/assertions/failedEmbeddedResources — blazemeter_reporter.py:695-717 | proven split logic exists | `build_failed_transactions` mirrors __add_errors split | USE |
| artifact file → upload | `failed_transactions.json` in artifacts_dir | anything in artifacts_dir gets zipped+uploaded — blazemeter_reporter.py:268-298 | proven upload path | none needed | USE |

## Design Alternatives Considered (carried from brownfield)

| Option | Decision | Reason |
|---|---|---|
| A: Extract classification into shared helper, new Selenium reader | **SELECTED** | Reuses battle-tested categorization; keeps JTLErrorsReader JMeter-specific |
| C: Generate json in BlazeMeterUploader.post_process | **SELECTED** | Natural home for BlazeMeter-specific output; existing upload path |
| B: Generate in ConsolidatingAggregator | REJECTED | Aggregator is load-test focused; mixes concerns |
| D: New standalone reporter | MAYBE (deferred) | Extra module; only if uploader approach becomes unwieldy |
| E: Let a.blazemeter.com generate from raw data | REJECTED | Contradicts ticket scope ("Taurus-side: generate a json file") |

## Requirement Traceability Matrix

| Jira AC | Spec FR/SC | Plan section | Task IDs | Test | Impl file(s) |
|---|---|---|---|---|---|
| Taurus generates generic EFT json artifact | FR-001, FR-002, SC-001 | Approach §4, Data Model | T004, T005, T006, T007, T008 | test_build_failed_transactions_shape, test_artifact_written_when_flag_on, test_empty_artifact_when_no_failures | bzt/modules/blazemeter/blazemeter_reporter.py, bzt/modules/eft.py |
| Selenium error categorization (general vs assertion) | FR-003, FR-008, SC-003 | Approach §2 | T002, T009, T011 | test_classify_general_vs_assertion, test_eft_assertion_probe | bzt/modules/eft.py, bzt/modules/functional.py |
| Opt-in config flag default off | FR-006, SC-002 | Approach §1 | T013, T014 | test_no_artifact_when_flag_off | bzt/modules/blazemeter/blazemeter_reporter.py |
| Graceful degradation (empty responseBodies, synthetic assertion name) | FR-004, FR-005 | Approach §3, §5 | T010, T012, T015, T016 | test_synthetic_assertion_name_fallback, test_empty_response_bodies_and_null_rc | bzt/modules/eft.py |
| Do not modify JTLErrorsReader / do not gate in taurus | FR-007, FR-008 | Approach §2 | T019 | N/A — nose2 suite | bzt/modules/jmeter.py (unchanged), bzt/modules/eft.py |

## Project Structure

### Documentation (this feature)

```text
specs/mob-43135/
├── plan.md              # This file
├── spec.md
├── brownfield-context.md
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (json schema of the artifact)
├── design-contract.json # planner structured contract
├── evidence-index.json
└── tasks.md             # Phase 2 output (/speckit-tasks)
```

### Source Code (repository root)

```text
bzt/
├── modules/
│   ├── eft.py                        # NEW — shared error classification helper + artifact builder
│   ├── blazemeter/
│   │   └── blazemeter_reporter.py    # MODIFY — post_process gains flag-gated EFT generation
│   ├── jmeter.py                     # UNCHANGED — classification logic is extracted, not moved
│   ├── functional.py                 # READ — FunctionalSample/ResultsTree source of truth
│   └── aggregator.py                 # READ — error_item_skel / ERRTYPE_* enums

tests/
└── unit/
    └── modules/
        ├── test_eft.py                       # NEW — helper + builder + synthetic-name fallback
        └── blazemeter/
            └── test_blazemeter_eft.py        # NEW — uploader integration (flag on/off, artifact written)

tests/resources/
└── selenium/
    └── eft_assertion_failure.ldjson          # NEW — realistic assertion-failure fixture (A-ASSERT POC)
```

**Structure Decision**: Single-project Taurus layout. New logic is isolated to `bzt/modules/eft.py`; the only existing-file modification is the flag-gated hook in `blazemeter_reporter.py`. `JTLErrorsReader`/`jmeter.py` stays untouched per FR-008.

## Complexity Tracking

> No Constitution Check violations — table intentionally empty.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| (none) | | |

## Failure Modes

- **No functional results available** (e.g. a pure-JMeter run with the flag on): the generator writes a well-formed artifact with empty collections rather than raising (degradation contract in the Performance note).
- **Assertion name not recoverable** from `FunctionalSample.error_msg`/`error_trace`: fall back to a synthetic name `assert::<label>`, still typed ERRTYPE_ASSERT (FR-004).
- **No response bodies for browser tests / null response code**: emit empty `responseBodies` and tolerate null `rc` (FR-005).
- **Config flag off/absent**: skip generation entirely — zero behaviour change, no error raised (FR-006).
- **Write failure to artifacts_dir**: log through the module logger and continue (must not crash the run's post_process); the missing artifact simply means no EFT data for that run.

## Non-Goals

- Not modifying or repurposing `JTLErrorsReader` / `bzt/modules/jmeter.py` for non-JMeter formats — the classification logic is extracted into a shared helper (FR-008).
- Not gating the EFT capability by executor type or account flag inside taurus — gating stays server-side in a.blazemeter.com (FR-007).
- Not changing default behaviour when the flag is off.
- Not implementing the a.blazemeter.com backend gating/ingestion changes — those belong to the a.blazemeter.com repo's plan.
- Not finalizing the json schema — the initial format is used pending Sparta sign-off.
