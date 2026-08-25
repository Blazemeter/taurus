# Research: EFT for Browser-Based Tests — Taurus Side (MOB-43135)

All unknowns from Technical Context are resolved below. Sources are cited to the brownfield research and code.

## Decision: Where to generate the artifact
- **Decision**: Generate `failed_transactions.json` inside `BlazeMeterUploader.post_process()`.
- **Rationale**: The uploader already owns BlazeMeter-specific output and the `__upload_artifacts` path; anything written to `engine.artifacts_dir` before upload is bundled automatically (`bzt/modules/blazemeter/blazemeter_reporter.py:268-298`). This avoids new upload logic.
- **Alternatives considered**: ConsolidatingAggregator (rejected — load-test focused, mixes concerns); a new standalone reporter (deferred — extra module, only if the uploader path becomes unwieldy); a.blazemeter.com-side generation (rejected — contradicts ticket scope).

## Decision: How to categorize Selenium errors
- **Decision**: Reuse the existing ERRTYPE_ERROR / ERRTYPE_ASSERT / ERRTYPE_SUBSAMPLE enums (`bzt/modules/aggregator.py:225-227`) by extracting the classification logic currently inlined in `JTLErrorsReader.find_failure` (`bzt/modules/jmeter.py:1413-1454`) into a new shared helper (`bzt/modules/eft.py`).
- **Rationale**: Wrapper-fidelity (constitution II) — reuse the battle-tested categorization; keep `JTLErrorsReader` JMeter-specific (Negative Constraint / FR-008).
- **Alternatives considered**: repurposing `JTLErrorsReader` for LDJSON (rejected — tightly coupled to JTL XML; would risk JMeter regressions).

## Decision: Source of truth for Selenium transactions
- **Decision**: Read `FunctionalAggregator.cumulative_results` (`ResultsTree` of `FunctionalSample`) at post_process.
- **Rationale**: `functional.py:47-128` shows `process_readers` populates `cumulative_results` in `check` and `post_process`; the tree is in memory and available at the read point (Runtime Data Availability Proof — PROVEN, A-RUNTIME).
- **Alternatives considered**: parsing LDJSON files directly (rejected — the aggregator is the canonical in-memory source; file parsing duplicates work).

## Decision: Assertion-name recovery (VALIDATE)
- **Decision**: Attempt to recover the assertion name from `FunctionalSample.error_msg`/`error_trace`; if unrecoverable, synthesize `assert::<label>`. Classify as ERRTYPE_ASSERT in both cases.
- **Rationale**: JMeter recovers assertion name from an XML tag (`jmeter.py:1413-1453`); Apiritif/Selenium may only embed it in a traceback string. This is UNPROVEN at source level (A-ASSERT). A POC/contract task must read a real assertion-failure fixture and prove recoverability; the synthetic fallback guarantees correctness regardless.
- **Alternatives considered**: always synthesize (rejected — loses the real name when it is available); block on this (rejected — reviewer handoff accepts Sparta approval as non-blocking; the fallback removes the blocking risk).

## Decision: Config surface
- **Decision**: `modules.blazemeter.generate-failed-transactions` (bool, default `False`), read via `self.settings.get(...)` in the uploader.
- **Rationale**: YAML-first (constitution I); resolved via Slack (option A) — opt-in, default off, safest rollout.
- **Alternatives considered**: always-on (rejected via Slack); default-on-for-Selenium (rejected via Slack).

## Decision: Graceful degradation
- **Decision**: `responseBodies` → empty list; `rc` → `None` tolerated; no failures → well-formed empty artifact.
- **Rationale**: Browser tests have no JMeter-style response bodies (Negative Constraint); the consumer must still receive a parseable file.

## Decision: Sparta schema dependency
- **Decision**: Use an initial format mirroring the proven `error_item_skel` / `DatapointSerializer.__add_errors` split; track Sparta final sign-off as a non-blocking dependency task.
- **Rationale**: task-reviewer handoff explicitly records Sparta approval as non-blocking; mirroring the proven shape keeps final migration to a schema-mapping change (SC-004).
