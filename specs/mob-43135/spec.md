# Feature Specification: Exclude Failed Transactions (EFT) for Browser-Based Tests — Taurus Side

**Feature Branch**: `ai-mob-43135`
**Created**: 2026-08-25
**Status**: Draft
**Input**: Jira MOB-43135 — Support Exclude Failed Transactions on browser-based tests (Taurus/Selenium slice)

## Background / Overview

Exclude Failed Transactions (EFT) lets customers exclude failed transactions from request-stats/timeline data. Today it works only for JMeter/yaml-jmx scripts. Browser-based performance-test customers (e.g. Australian Post, Mastercard) whose applications cannot use JMeter cannot use EFT. This Epic spans the Sparta (Taurus) and Titans (Selenium) teams. This spec covers the **Taurus (`bzt`)** slice: generate a generic EFT-compatible json artifact independent of executor type, and categorize Selenium/Apiritif errors into general errors vs. failed assertions.

Sparta owns the final json format. Per the task-reviewer handoff this is a **non-blocking** dependency — Titans implement against an initial format now; the final format requires Sparta review before finalization.

## Field Semantics (BINDING)

> Source: brownfield semantic disambiguation (brownfield-context.md § Semantic Disambiguation / Binding Decisions).
> These decisions are NON-NEGOTIABLE. Every downstream artifact — plan.md, tasks.md, tests, and the implementation — must honor them.

### Approved Identifiers (USE these for this ticket)
- `KPISet.ERRTYPE_ERROR` / `KPISet.ERRTYPE_ASSERT` / `KPISet.ERRTYPE_SUBSAMPLE` — existing error-type enums (`bzt/modules/aggregator.py:225-227`); reuse for Selenium error categorization.
- `FunctionalSample` + `ResultsTree` (`bzt/modules/functional.py:67-128`) — source of truth for per-transaction pass/fail and error details for Selenium/Apiritif.
- `BlazeMeterUploader.post_process()` / `__upload_artifacts()` (`bzt/modules/blazemeter/blazemeter_reporter.py:268-298`) — existing artifact upload path; the EFT json follows the same mechanism (write to `engine.artifacts_dir`, bundled into `artifacts.zip`).
- `generate-failed-transactions` — new opt-in config flag under `modules.blazemeter`, default off (resolved via Slack, option A).

### Forbidden Identifiers (DO NOT USE — belong to a different feature)
none

### Negative Constraints
- **DO NOT** hard-code the final EFT JSON schema in taurus until Sparta approves; design the initial format to mirror the proven `error_item_skel` / `DatapointSerializer` split so migration is cheap.
- **DO NOT** modify `JTLErrorsReader` to produce non-JMeter output; keep it JMeter-specific and extract/reuse only the error-categorization logic (ERRTYPE_* enums and type classification).
- **DO NOT** upload the EFT JSON separately from `artifacts.zip`; include it as a file within the artifacts dir (`failed_transactions.json`).
- **DO NOT** gate the EFT feature inside taurus; gating lives in a.blazemeter.com (`Master::isTransactionFilterAllowed`). Taurus emits the artifact whenever the config flag is set.
- **DO NOT** assume Selenium errors carry HTTP response codes (`rc` may be null); handle gracefully. Browser tests have no JMeter-style response bodies — emit empty `responseBodies`.

## Current Implementation

> Source: brownfield codebase research (brownfield-context.md)

### What exists today
- The existing jmx-based EFT error output is the analog to mirror. JMeter errors are read by `JTLErrorsReader` (`bzt/modules/jmeter.py:1255-1454`) and categorized via `find_failure` into ERRTYPE_ERROR / ERRTYPE_ASSERT / ERRTYPE_SUBSAMPLE.
- Error structure `error_item_skel` (`bzt/modules/aggregator.py:271-279`) has keys: `cnt`, `msg`, `tag` (assertion name if ERRTYPE_ASSERT), `rc`, `type` (0/1/2), `urls`, `responseBodies`.
- `DatapointSerializer.__add_errors` (`bzt/modules/blazemeter/blazemeter_reporter.py:695-717`) splits errors by type into `errors` / `assertions` (name + failureMessage) / `failedEmbeddedResources`.
- Selenium/Apiritif functional results flow through `FunctionalSample` / `ResultsTree` / `FunctionalAggregator` (`bzt/modules/functional.py:47-128`); transaction lifecycle via `subscribe_to_transactions` (`bzt/modules/_selenium.py:66-68`, `bzt/modules/_apiritif/executor.py:231-248`).
- Artifacts are uploaded by `BlazeMeterUploader.post_process()` → `__upload_artifacts()`; everything in `engine.artifacts_dir` is zipped and uploaded.
- **No EFT JSON is currently generated inside taurus** — the taurus side is new work.

### Key files the planner/implementer must read
- `bzt/modules/aggregator.py` (error_item_skel, ERRTYPE_* enums)
- `bzt/modules/jmeter.py` (JTLErrorsReader.find_failure — classification logic to reuse)
- `bzt/modules/functional.py` (FunctionalSample, ResultsTree, FunctionalAggregator)
- `bzt/modules/blazemeter/blazemeter_reporter.py` (post_process, __upload_artifacts, DatapointSerializer.__add_errors)
- `bzt/modules/_selenium.py`, `bzt/modules/_apiritif/executor.py`, `bzt/modules/_apiritif/generator.py` (Selenium/Apiritif transaction + assertion handling)

## Runtime Data Availability (BINDING)

> Source: brownfield-context.md § Runtime Data Availability Proof. All critical runtime data is available at the post_process read point for both JMeter and Selenium.

- Per-transaction Selenium/Apiritif error list, labels, and start/end times are held in `FunctionalAggregator.cumulative_results` (ResultsTree) until `post_process` completes — **available** at the read point.
- Assertion failure name/message/stacktrace live in `FunctionalSample.error_msg` / `error_trace` — **available but "maybe" structured**: the assertion *name* may not be reliably separable from the traceback. This is a VALIDATE assumption (see Assumptions) with a POC/contract-verification task; degrade to a synthetic name derived from the transaction label if the real name cannot be recovered.
- Error response bodies for browser tests are **not** produced JMeter-style — emit empty `responseBodies`.

## Cross-Repo Capability Analysis (BINDING)

- **USE** a.blazemeter.com `Master::isTransactionFilterAllowed()` gating — must add `SCRIPT_TYPE_SELENIUM` (tracked in the a.blazemeter.com plan, not here).
- **USE** BlazeMeterUploader artifact upload mechanism — no new upload logic in taurus.
- **MAYBE** a.blazemeter.com external-results-import endpoint (route exists but `@deprecated`; handler unverified — a.blazemeter.com repo verifies).
- **NEEDS_APPROVAL** Sparta final json format — initial format used now.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Generate a generic EFT json artifact for Selenium runs (Priority: P1)

A customer runs a browser-based (Selenium/Apiritif) performance test through Taurus with the BlazeMeter uploader and `modules.blazemeter.generate-failed-transactions: true`. Taurus produces a `failed_transactions.json` artifact whose error structure mirrors the existing jmx-based EFT output, so the BlazeMeter backend can later exclude failed transactions from stats.

**Why this priority**: This is the core deliverable — without the artifact there is nothing for the backend to filter on. It is the minimum viable slice.

**Independent Test**: Run a Selenium test fixture through the engine with the flag on; assert `failed_transactions.json` is written to `engine.artifacts_dir` with the three-way error split (`errors` / `assertions` / `failedEmbeddedResources`) keyed by ERRTYPE_*.

**Acceptance Scenarios**:

1. **Given** a Selenium run with the flag on and at least one failed transaction, **When** the engine reaches post_process, **Then** `failed_transactions.json` is written to `engine.artifacts_dir` and included in the artifacts upload.
2. **Given** the flag is off (default), **When** the engine reaches post_process, **Then** no `failed_transactions.json` is generated.
3. **Given** a Selenium run with only passing transactions, **When** post_process runs with the flag on, **Then** the artifact is written with empty error collections (well-formed, no failures).

---

### User Story 2 - Categorize Selenium errors into general errors vs. failed assertions (Priority: P1)

The json artifact must classify each failure as a general error or a failed assertion (e.g. "Assert X" actions), reusing the existing ERRTYPE_* enum classification, so the backend and UI can distinguish the two.

**Why this priority**: Categorization is an explicit Jira acceptance criterion and the second functional pillar; the artifact is only useful if failures are correctly typed.

**Independent Test**: Feed a Selenium fixture containing both a general error (e.g. an element-not-found failure) and a failed assertion; assert the general error lands under `errors` (ERRTYPE_ERROR) and the assertion under `assertions` (ERRTYPE_ASSERT) with a name and failureMessage.

**Acceptance Scenarios**:

1. **Given** a Selenium transaction with a failed assertion, **When** the artifact is generated, **Then** the failure appears under `assertions` with `type` = ERRTYPE_ASSERT and a non-empty assertion name.
2. **Given** a Selenium transaction with a non-assertion error, **When** the artifact is generated, **Then** the failure appears under `errors` with `type` = ERRTYPE_ERROR.
3. **Given** a failed assertion whose assertion name cannot be recovered from `error_msg`/`error_trace`, **When** the artifact is generated, **Then** a synthetic name derived from the transaction label is used and the failure is still typed ERRTYPE_ASSERT.

---

### User Story 3 - Opt-in configuration, default off (Priority: P2)

An operator enables EFT json generation explicitly via `modules.blazemeter.generate-failed-transactions: true`. Default behaviour is unchanged (off), so existing runs are unaffected.

**Why this priority**: Safe rollout — the feature must not change behaviour for the vast majority of existing Selenium runs that do not opt in.

**Independent Test**: Load a config with and without the flag; assert generation fires only when the flag is truthy.

**Acceptance Scenarios**:

1. **Given** a config without the flag, **When** post_process runs, **Then** no artifact is generated and no error is raised.
2. **Given** a config with `generate-failed-transactions: true`, **When** post_process runs, **Then** the artifact is generated.

---

### Edge Cases

- What happens when the run has failed transactions but the assertion name is embedded only in the traceback string? → Fall back to a synthetic name derived from the transaction label (US2 scenario 3).
- What happens when the executor is not Selenium (e.g. a mixed JMeter+Selenium run)? → The generic path emits the artifact from available functional/KPI data; JMeter's own EFT path is unchanged (Negative Constraint: do not modify JTLErrorsReader).
- What happens when there are no failed transactions? → A well-formed artifact with empty error collections is written (US1 scenario 3).
- What happens when `responseBodies` cannot be captured for browser tests? → Emit empty `responseBodies` (graceful degradation).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Taurus MUST generate a `failed_transactions.json` artifact in `engine.artifacts_dir` during `post_process`, bundled into the standard `artifacts.zip` upload, when `modules.blazemeter.generate-failed-transactions` is truthy.
- **FR-002**: The artifact structure MUST mirror the proven jmx EFT output — a three-way error split (`errors` / `assertions` / `failedEmbeddedResources`) keyed by ERRTYPE_ERROR / ERRTYPE_ASSERT / ERRTYPE_SUBSAMPLE, with per-error fields consistent with `error_item_skel` (`cnt`, `msg`, `tag`, `rc`, `type`, `urls`, `responseBodies`).
- **FR-003**: Selenium/Apiritif failures MUST be categorized into general errors vs. failed assertions by reusing the existing ERRTYPE_* classification, sourced from `FunctionalSample` / `ResultsTree`.
- **FR-004**: When a failed assertion's name cannot be recovered from `FunctionalSample.error_msg` / `error_trace`, the system MUST fall back to a synthetic name derived from the transaction label, still typed ERRTYPE_ASSERT.
- **FR-005**: For browser-based tests, the system MUST emit an empty `responseBodies` collection rather than failing when response bodies are unavailable, and MUST tolerate a null `rc`.
- **FR-006**: Generation MUST be opt-in via a new `modules.blazemeter.generate-failed-transactions` config flag defaulting to off; when off, behaviour MUST be unchanged and no artifact is produced.
- **FR-007**: Taurus MUST NOT gate the EFT feature by executor type or account flag; gating remains server-side (a.blazemeter.com).
- **FR-008**: The error-classification logic MUST be reusable/extracted such that JMeter's `JTLErrorsReader` is not repurposed for non-JMeter formats (extract classification, keep concrete readers executor-specific).

### Key Entities

- **Failed Transactions Artifact (`failed_transactions.json`)**: The per-run EFT output. Contains report metadata and a per-transaction list, each with categorized error collections (`errors` / `assertions` / `failedEmbeddedResources`).
- **Error Item**: A single categorized failure with `cnt`, `msg`, `tag` (assertion name or null), `rc` (or null), `type` (ERRTYPE_*), `urls`, `responseBodies`.
- **Transaction Record**: Per-transaction `label`, `timestamp`, `duration`, and its categorized error collections, sourced from `FunctionalSample` / `ResultsTree`.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A Selenium test run with the flag enabled and at least one failed transaction produces a well-formed `failed_transactions.json` that a validator can parse, with 100% of failures categorized into exactly one of the three error collections.
- **SC-002**: With the flag disabled (default), 0 artifacts are produced and no behavioural change is observed versus the pre-feature baseline.
- **SC-003**: 100% of failed assertions in the artifact carry a non-empty assertion name (real when recoverable, synthetic otherwise) and are typed as assertions, not general errors.
- **SC-004**: The initial json format matches the proven jmx EFT structure closely enough that adopting the Sparta-approved final format requires only a schema-mapping change, not a data-flow rewrite.

## Scope

### In Scope
- Taurus-side generic EFT-compatible json file (`failed_transactions.json`) generation independent of executor type (Selenium first).
- Selenium/Apiritif error categorization into general errors vs. failed assertions (reusing ERRTYPE_*).
- New opt-in config flag `modules.blazemeter.generate-failed-transactions` (default off).

### Out of Scope
- Playwright support — deferred until Playwright integration is complete.
- a.blazemeter.com backend gating change (`Master::isTransactionFilterAllowed` adding `SCRIPT_TYPE_SELENIUM`) — handled in the a.blazemeter.com repo plan.
- Final json schema sign-off — owned by Sparta (Taurus) team; this spec uses an initial format.

### Non-Goals
- This feature does NOT change or repurpose `JTLErrorsReader` / `bzt/modules/jmeter.py` for non-JMeter formats.
- This feature does NOT gate the EFT capability by executor type or account flag inside taurus (gating stays server-side).
- This feature does NOT alter default behaviour when the config flag is off.

## Failure Modes

- **No functional results available** (e.g. a pure-JMeter run with the flag on): write a well-formed artifact with empty collections rather than raising.
- **Assertion name not recoverable** from `error_msg`/`error_trace`: fall back to a synthetic name `assert::<label>`, still typed ERRTYPE_ASSERT (FR-004).
- **No response bodies for browser tests**: emit empty `responseBodies`; tolerate null `rc` (FR-005).
- **Config flag off / absent**: no artifact produced, no error raised (FR-006).

## Requirement Traceability Matrix

| Jira AC | Spec FR/SC | Plan section | Task IDs | Test | Impl file(s) |
|---|---|---|---|---|---|
| Taurus generates generic EFT json artifact | FR-001, FR-002, SC-001 | plan Approach §4 / Data Model | T004, T005, T006, T007, T008 | test_build_failed_transactions_shape, test_artifact_written_when_flag_on, test_empty_artifact_when_no_failures | bzt/modules/blazemeter/blazemeter_reporter.py, bzt/modules/eft.py |
| Selenium error categorization (general vs assertion) | FR-003, FR-008, SC-003 | plan Approach §2 | T002, T009, T011 | test_classify_general_vs_assertion, test_eft_assertion_probe | bzt/modules/eft.py |
| Opt-in config flag default off | FR-006, SC-002 | plan Approach §1 | T013, T014 | test_no_artifact_when_flag_off | bzt/modules/blazemeter/blazemeter_reporter.py |
| Graceful degradation (empty responseBodies, synthetic assertion name) | FR-004, FR-005 | plan Approach §3, §5 | T010, T012, T015, T016 | test_synthetic_assertion_name_fallback, test_empty_response_bodies_and_null_rc | bzt/modules/eft.py |
| Do not modify JTLErrorsReader / do not gate in taurus | FR-007, FR-008 | plan Approach §2 | T019 | N/A — nose2 suite | bzt/modules/jmeter.py (unchanged), bzt/modules/eft.py |

## Assumptions

- **A-ASSERT (VALIDATE)**: Assertion names for Selenium/Apiritif failures can be recovered from `FunctionalSample.error_msg`/`error_trace`. This is UNPROVEN at the source level and MUST be validated via a POC/contract-verification task that reads a real assertion-failure fixture. If unrecoverable, the synthetic-name fallback (FR-004) applies. Risk: medium.
- **A-SPARTA (VALIDATE/dependency)**: The initial json format mirroring the jmx EFT structure is acceptable to Sparta as a starting point; final sign-off is a tracked non-blocking dependency (per task-reviewer handoff).
- **A-UPLOAD (PROVEN)**: Placing `failed_transactions.json` in `engine.artifacts_dir` results in it being uploaded via the existing `__upload_artifacts` path (blazemeter_reporter.py:268-298).
- **A-RUNTIME (PROVEN)**: Functional transaction/error data is available in `FunctionalAggregator.cumulative_results` at post_process time (functional.py:47-128).
- Playwright support is out of scope for this feature.
- a.blazemeter.com backend gating and ingestion changes are handled in that repo's plan, not here.
