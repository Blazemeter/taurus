# Feature Specification: Spike — Research "SKIP ON FAILURE" toggle for BBT tests (taurus)

**Feature Branch**: `ai-mob-53697`
**Created**: 2026-09-03
**Status**: Draft (Spike / Research)
**Input**: MOB-53697 — Spike to research a "SKIP ON FAILURE" toggle for BBT (browser-based / functional) tests. When enabled, once a step fails, the remaining steps of that iteration are skipped so cascading failures don't inflate error rates in Request Stats. Requested by Independent Health.

> **This is a SPIKE / research ticket.** No production code ships from MOB-53697. The deliverable is documented, code-grounded answers to four research questions plus a follow-up implementation Story. Functional requirements below describe what the RESEARCH must produce from **taurus's** perspective, not shipped runtime behavior. Implementation is deferred to a follow-up Story against Epic MOB-47570.

## Background / Overview

BlazeMeter customers running browser-based (BBT) / functional tests via Taurus experience "cascading failures": when an early step in an iteration fails, later steps that depend on it also fail, inflating the error rate shown in Request Stats. Customer Independent Health has requested a **SKIP ON FAILURE** toggle applied at the whole-scenario/test level. When on, the first failure in an iteration causes the remaining steps of that iteration to be skipped (and ideally marked "skipped" rather than "failed"), so error rates reflect only real, independent failures.

Taurus is the test-execution framework that generates the functional test code and dispatches it to the `apiritif.loadgen` runner. This spike researches **where and how** that skip behavior would be implemented within the taurus code path, and answers four open questions the team needs settled before scoping the implementation Story.

## Field Semantics (BINDING)

> Source: brownfield semantic disambiguation (`brownfield-context.md` § Semantic Disambiguation). These decisions are NON-NEGOTIABLE for the follow-up implementation Story.

| Ticket Identifier | Codebase Symbol | Decision | Evidence |
|---|---|---|---|
| SKIP ON FAILURE (config flag) | (none — new) | NEW → introduce `skip_on_failure` | no existing collision |
| iteration | `load.iterations` | USE | `bzt/modules/_apiritif/executor.py:141-147` |
| SKIPPED (status) | `TEST_STATUSES` includes `"SKIPPED"` | USE | `bzt/modules/functional.py:166` |

### Approved Identifiers (USE)
- `skip_on_failure` — new snake_case scenario-level flag; matches ticket intent; no collision.
- `SKIPPED` — existing `FunctionalSample` status value; reuse for skipped steps.
- `iteration` — established load concept; one full run of the generated test method.

### Forbidden Identifiers
none — brownfield research found no naming/semantic collisions.

### Documented Ambiguities (spike open items — for the follow-up Story to settle)
- `status_per_iteration` — functional samples are recorded per test case (per transaction/step), not explicitly per iteration-loop. The follow-up Story must decide the granularity at which a "skipped" marker is emitted. Evidence: `bzt/modules/functional.py:67-92`.

## Current Implementation

> Source: brownfield codebase research (`brownfield-context.md`).

### What exists today
- Iterations are applied via `load.iterations`, passed as `--iterations N` to the external `apiritif.loadgen` subprocess. Taurus does **not** own the iteration loop. (`bzt/modules/_apiritif/executor.py:141-147, 186-207`)
- The generated master test method calls step methods sequentially and already wraps them in `try/finally` when teardown methods exist. (`bzt/modules/_apiritif/generator.py:2020-2046`)
- Per-step/per-transaction status is recorded as `FunctionalSample.status`; `SKIPPED` is already a valid value. (`bzt/modules/functional.py:68-92, 166`)
- `LoadSamplesReader` deliberately drops `SKIPPED` samples so they don't skew load KPIs; `FuncSamplesReader` keeps them. (`bzt/modules/functional.py:264-265, 359-365`)

### Root cause / gap
No logic today detects a step failure mid-iteration and skips the remaining steps. Adding it requires code-generation changes (generator wraps step calls with early-exit-on-failure) plus a scenario-config flag read by the apiritif executor.

## User Scenarios & Testing *(spike-framed)*

### User Story 1 — Research questions answered with code evidence (Priority: P1)

The team needs authoritative, code-grounded answers to four questions so the implementation Story can be scoped with confidence rather than guesswork.

**Why this priority**: This IS the spike deliverable. Without answers, the implementation Story cannot be estimated or designed.

**Independent Test**: Each of the four questions has a written answer citing `file:line` evidence in the taurus codebase, reviewable by an engineer without running code.

**Acceptance Scenarios**:
1. **Given** the taurus functional/BBT execution path, **When** the research is complete, **Then** Q1 (how load/iterations are applied) is answered with evidence at `bzt/modules/_apiritif/executor.py`.
2. **Given** the generated test code, **When** the research is complete, **Then** Q2 (how to skip the rest of the iteration on failure) names the exact generator entry point (`_gen_master_test_method`) and the recommended approach.
3. **Given** the functional data model, **When** the research is complete, **Then** Q3 (is per-iteration status set; can we mark "skipped") is answered including the `SKIPPED` status and the load-vs-functional reader distinction.
4. **Given** the iteration loop's ownership, **When** the research is complete, **Then** Q4 (does skipping an iteration skip the whole test) is answered "no" with evidence, and the residual dependency risk on `apiritif.loadgen` is flagged.

---

### User Story 2 — Follow-up implementation Story filed (Priority: P1)

**Why this priority**: The spike's second acceptance criterion; converts findings into actionable scoped work.

**Independent Test**: A follow-up Story exists against Epic MOB-47570 capturing the SKIP ON FAILURE feature scope, informed by this spike.

**Acceptance Scenarios**:
1. **Given** the completed research, **When** the spike closes, **Then** a follow-up implementation Story is filed against Epic MOB-47570.

---

### Edge Cases

- What if `apiritif.loadgen` auto-aborts all remaining iterations when the first iteration raises? Then the feature (skip *this* iteration, continue the *next*) cannot work as stated and the approach must change. Flagged as residual risk FR-005.
- What if the customer later needs this for load-mode (not just functional) tests? `LoadSamplesReader` currently drops `SKIPPED` samples by design; a load-mode variant would need a different treatment. Out of scope for the initial feature; documented for the follow-up.

## Requirements *(spike deliverables)*

### Functional Requirements

- **FR-001**: The research MUST answer Q1 — how load/iterations are currently applied to functional/BBT tests — with taurus `file:line` evidence.
- **FR-002**: The research MUST answer Q2 — how the remaining steps of an iteration can be skipped on failure — identifying the generator/executor code that would change.
- **FR-003**: The research MUST answer Q3 — whether per-iteration scenario/label/step status is set, and whether a step can be marked `SKIPPED` — with evidence, including the load-reader-drops-SKIPPED caveat.
- **FR-004**: The research MUST answer Q4 — whether skipping an iteration skips the whole test (expected: no) — with evidence about iteration-loop ownership.
- **FR-005**: The research MUST record the residual risk that `apiritif.loadgen` (an external dependency) may auto-abort remaining iterations on failure, and recommend a validation step.
- **FR-006**: A follow-up implementation Story MUST be filed against Epic MOB-47570 capturing the SKIP ON FAILURE feature scope informed by the findings.
- **FR-007**: All findings and answers MUST be documented on MOB-53697 (Jira comment or linked doc).

### Key Entities

- **Research finding** — a question/answer pair with code evidence (`file:line`), a decision (USE/NEW/AMBIGUOUS), and any residual risk.
- **Follow-up Story** — the scoped implementation ticket derived from the spike, linked to Epic MOB-47570.

## Success Criteria *(spike)*

### Measurable Outcomes

- **SC-001**: 4 of 4 research questions have a documented answer with at least one `file:line` code citation each.
- **SC-002**: 0 unresolved planner-blocking unknowns remain for the SPIKE deliverable (open items are documented as recommendations for the follow-up Story, not blockers on this ticket).
- **SC-003**: 1 follow-up implementation Story is filed against Epic MOB-47570.
- **SC-004**: The residual `apiritif.loadgen` iteration-abort risk is explicitly documented with a recommended validation step.

## Assumptions

- The customer use case (Independent Health) is BBT/Selenium functional tests, so the `FuncSamplesReader` path (which keeps `SKIPPED`) is the relevant one; load-mode support is out of scope for the initial feature.
- Implementation lives primarily in taurus's generated code path; scenario config (`skip_on_failure`) is read by the apiritif executor. Whether the API (`a.blazemeter.com`) or cloud layer (`taurus-cloud`) also needs changes is examined in those repos' own spike slices.

## In Scope

- Answering the 4 research questions from taurus's perspective, with `file:line` evidence.
- Identifying the code that would change for a future implementation.
- Documenting risks and open items.

## Out of Scope

- Any production code change.
- Action-level override of the setting (future enhancement, only if real demand arises).
- Actual implementation of the SKIP ON FAILURE toggle (deferred to the follow-up Story).

## Dependencies

- External Python package `apiritif` (`apiritif.loadgen`) owns the iteration loop — its abort-on-failure behavior is a dependency the follow-up Story must validate.
- Related repos for the full feature: `a.blazemeter.com` (config/API surface), `taurus-cloud` (cloud orchestration) — covered by their own spike slices.

## Requirement Traceability Matrix

| Jira AC | Spec (FR/SC) | Plan section | Task | Test | Impl file (future) |
|---|---|---|---|---|---|
| AC-1: All 4 research questions answered and documented | FR-001..FR-005, FR-007, SC-001, SC-002, SC-004 | Research Findings; Residual Risk | T002, T003, T004, T005, T006, T008 | N/A — spike documentation gate | N/A (no code ships) |
| AC-2: Follow-up implementation Story filed against Epic MOB-47570 | FR-006, SC-003 | Recommended Implementation Approach | T007 | N/A — Jira gate | N/A (no code ships) |

> Quality-gate note: this is a research spike; no AC is satisfied by a unit-testable behavior, so every `Test` cell is a documentation/Jira gate (`N/A — …`) and contains no `test_*` token by design.
