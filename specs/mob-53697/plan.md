# Implementation Plan: Spike — SKIP ON FAILURE research (taurus)

**Branch**: `ai-mob-53697` | **Date**: 2026-09-03 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `specs/mob-53697/spec.md`

> **SPIKE / RESEARCH PLAN.** This plan documents the code-grounded answers to the four research questions and the **recommended implementation approach for a follow-up Story**. It ships NO production code from MOB-53697. Every "would change" statement is a recommendation for the follow-up Story, not a change made now.

## Summary

Research whether/where the "SKIP ON FAILURE" behavior (skip remaining steps of a functional/BBT iteration once a step fails) can live in taurus. Findings: it is feasible via taurus's generated-test-code path; the `SKIPPED` status and iteration machinery already exist; no naming collisions. The only material risk is the external `apiritif.loadgen` iteration-abort behavior, which the follow-up Story must validate before committing to the approach.

## Technical Context

**Language/Version**: Python 3.x (Taurus `bzt`)
**Primary Dependencies**: `apiritif` (`apiritif.loadgen` runner — owns the iteration loop), Selenium/Apiritif executor stack
**Storage**: N/A (result files: JSON-LD `.ldjson` functional samples; no DB)
**Testing**: `nose2` (`python -m nose2 -s tests/unit -v`), `BZTestCase` + `EngineEmul` fixtures
**Target Platform**: Linux (CLI / cloud container)
**Project Type**: Single project — CLI test-automation framework (library + `bzt` CLI)
**Performance Goals**: N/A — spike ships no runtime code; no nested external API calls, so the N×M performance gate does not apply
**Constraints**: Public open-source repo — any future change requires backward-compat care and external-contributor review (per constitution II, and the catalog note on `taurus`)
**Scale/Scope**: Research-only; the follow-up feature touches the functional/BBT generated-code path in one repo (plus config surface in sibling repos)

## Constitution Check

*GATE: no production code ships from this spike, so no principle is exercised at implementation level. Assessed for the recommended future approach:*

- **I. YAML-First Unified Interface** — PASS (recommended `skip_on_failure` is a scenario-level YAML key, reachable by a stable name; no tool-native leakage).
- **II. Wrapper Fidelity Over Reinvention** — PASS with a note: the recommended approach injects skip logic into generated test code rather than forking `apiritif.loadgen`; the residual risk (FR-005) is precisely about not reinventing the iteration loop. The follow-up Story must confirm apiritif cooperates rather than being reimplemented.
- **III. Test-First (NON-NEGOTIABLE)** — N/A for the spike (no code). The follow-up Story MUST land generator/executor tests against `BZTestCase`.
- **IV. Module Lifecycle Discipline** — PASS (recommendation stays within the apiritif executor/generator; no lifecycle change).
- **V. Observable, Aggregatable Results** — PASS (reuses existing `SKIPPED` functional sample status and `FuncSamplesReader`).

No violations. Complexity Tracking not required.

## Research Findings (the 4 spike questions)

> Full evidence in [`brownfield-context.md`](./brownfield-context.md) § Spike Answers.

### Q1 — How is load / iteration currently applied?
Iterations come from `load.iterations` and are passed as `--iterations N` to the external `apiritif.loadgen` subprocess. Taurus generates the test code but does NOT own the iteration loop.
Evidence: `bzt/modules/_apiritif/executor.py:141-147` (reads iterations), `bzt/modules/_apiritif/executor.py:186-207` (subprocess invocation).

### Q2 — How can we skip the rest of the iteration on failure?
Inject skip logic into the **generated** master test method so a step failure short-circuits the remaining step calls in that iteration. The generator entry point is `_gen_master_test_method()`, which already wraps step calls in `try/finally` when teardown exists.
Evidence: `bzt/modules/_apiritif/generator.py:2020-2046` (master method), `bzt/modules/_apiritif/generator.py:1933-2018` (existing action-id exception wrapping — proves try/except is already generated).

### Q3 — Is per-iteration scenario/label status set? Can we mark "skipped"?
Status is recorded per test case (per transaction/step) as `FunctionalSample.status`; `SKIPPED` is already a valid value. Functional reporting (`FuncSamplesReader`) keeps `SKIPPED`; load-mode reporting (`LoadSamplesReader`) deliberately drops it to avoid KPI skew.
Evidence: `bzt/modules/functional.py:68-92` (sample model), `bzt/modules/functional.py:166` (`TEST_STATUSES` includes `SKIPPED`), `bzt/modules/functional.py:264-265` (load reader drops SKIPPED), `bzt/modules/functional.py:359-365` (func reader keeps it).
Open item: granularity (per-step vs per-iteration marker) is `status_per_iteration` — for the follow-up Story to settle.

### Q4 — Does skipping the iteration mean skipping the whole test?
No. The iteration loop is owned by `apiritif.loadgen`; skipping remaining steps in iteration N does not prevent iteration N+1. Taurus aggregates results after all iterations complete.
Evidence: `bzt/modules/_apiritif/executor.py:186-207` (iterations passed to subprocess), `bzt/modules/_apiritif/executor.py:250-252` (check() reads aggregated results, no early-stop).

## Design

> Recommended implementation approach for the follow-up Story. Nothing here is built by MOB-53697.

### Recommended Implementation Approach

1. **Config surface**: add a scenario-level `skip_on_failure` boolean flag. In taurus it is read by the apiritif executor from the scenario config and threaded into code generation. The authoritative config/API surface (how a user toggles it in the product) belongs to `a.blazemeter.com`; taurus consumes what the scenario YAML carries.
2. **Skip logic**: modify `_gen_master_test_method()` (`bzt/modules/_apiritif/generator.py:2020-2046`) to, when `skip_on_failure` is set, wrap sequential step calls so the first failing step short-circuits the remaining step calls in that iteration (teardown still runs).
3. **Status marking**: mark skipped steps with the existing `SKIPPED` status (`bzt/modules/functional.py:166`) so `FuncSamplesReader` surfaces them and error rates in Request Stats are not inflated. Scope to the functional path (customer use case is BBT/Selenium); load-mode is out of scope initially.
4. **Backward compatibility**: default `skip_on_failure` off; generated code unchanged when the flag is absent (constitution I/II).

### Design Alternatives Considered

| Option | What changes | Pros | Cons / Risks | Evidence | Decision |
|---|---|---|---|---|---|
| A. Skip logic in generated code (try/except-and-break in `_gen_master_test_method`) | Generator emits early-exit-on-failure step sequence | No external dependency change; try/except already generated | Must confirm apiritif doesn't auto-abort iterations | `bzt/modules/_apiritif/generator.py:2020-2046`, `bzt/modules/_apiritif/generator.py:1933-2018` | **SELECTED** (recommended path) |
| B. New `apiritif.loadgen` runtime flag | apiritif package grows a skip-on-failure mode | Centralized loop control | Requires upstream apiritif change; violates wrapper-fidelity if forked | `bzt/modules/_apiritif/executor.py:186-207` | REJECTED — external dependency + fork risk |
| C. Executor short-circuit (taurus stops reading results early) | Executor drops later samples | Simple in taurus | Wrong semantics: steps still ran; error rates still inflated; doesn't actually skip | `bzt/modules/_apiritif/executor.py:250-252` | REJECTED — doesn't meet the requirement |

## Failure Modes

> Because this is a research spike, "failure modes" describe risks to the RESEARCH conclusion and to the recommended future approach, not runtime error handling (no code ships).

- **Residual risk — apiritif.loadgen iteration abort (FR-005)**: `apiritif.loadgen` is an external package that owns the iteration loop. If it auto-aborts ALL remaining iterations when the first iteration raises an exception, then "skip this iteration, continue the next" cannot work as stated, and Design Alternative A would need to change (e.g. raise a non-exception skip signal, or coordinate an apiritif change).
  - **Recommended validation** (for the follow-up Story, before committing to approach A): run `python -m apiritif.loadgen --iterations 2 <script>` where iteration 1 raises an exception, and observe whether iteration 2 executes. Document the outcome.
- **Failure mode — load-mode misapplication**: if the feature were naively applied to load-mode tests, `LoadSamplesReader` would drop the `SKIPPED` samples (`bzt/modules/functional.py:264-265`), so the intended error-rate improvement would not appear. Mitigation: scope the initial feature to the functional path only.
- **Failure mode — granularity ambiguity**: the `status_per_iteration` open item means the follow-up Story must explicitly choose per-step vs per-iteration marking; leaving it undecided risks inconsistent reporting.

## Non-Goals

- This spike does NOT implement the SKIP ON FAILURE toggle (deferred to the follow-up Story).
- This spike does NOT add or modify any production code in taurus.
- This spike does NOT design the action-level override (explicit future enhancement, out of scope).
- This spike does NOT cover load-mode support for skip-on-failure (functional/BBT only for the initial feature).

## Cross-Repo Boundary

- **taurus** (this slice): generated-code skip logic + reading `skip_on_failure` from scenario config; reuse `SKIPPED` status.
- **a.blazemeter.com**: owns the user-facing config/API surface that persists and delivers the `skip_on_failure` toggle into the scenario definition. (See that repo's spike slice.)
- **taurus-cloud**: owns how the test runs in the cloud; must confirm the scenario config propagates and no cloud-only override drops the flag. (See that repo's spike slice.)

## Project Structure

### Documentation (this feature)

```text
specs/mob-53697/
├── plan.md                 # This file
├── spec.md                 # Spike spec
├── brownfield-context.md   # Code-grounded research (Spike Answers)
├── brownfield-context.meta.json
├── research.md             # Phase 0 consolidation (this spike)
├── data-model.md           # Phase 1 (research entities)
├── quickstart.md           # Phase 1 (how to validate findings)
├── tasks.md                # Research/documentation tasks
├── test-scenarios.md       # Spike verification scenarios (no unit tests)
├── design-contract.json
└── evidence-index.json
```

**Structure Decision**: Single-project CLI framework. No new source directories introduced by this spike; the recommendation (future) touches `bzt/modules/_apiritif/generator.py` and `bzt/modules/_apiritif/executor.py`.

## Complexity Tracking

> No Constitution Check violations — table intentionally empty.
