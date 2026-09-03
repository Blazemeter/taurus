# Phase 0 Research — MOB-53697 (taurus)

## Decision: skip logic belongs in generated test code
- **Decision**: Implement SKIP ON FAILURE by injecting early-exit-on-failure into the generated master test method (`_gen_master_test_method`), gated by a scenario-level `skip_on_failure` flag.
- **Rationale**: Taurus does not own the iteration loop (apiritif.loadgen does); the generated code is the only place taurus controls per-step sequencing within an iteration. try/except is already generated (`generator.py:1933-2018, 2020-2046`).
- **Alternatives considered**: (B) new apiritif runtime flag — rejected, external dependency/fork risk; (C) executor short-circuit — rejected, wrong semantics (steps still ran).

## Decision: reuse SKIPPED status, functional path only
- **Decision**: Mark skipped steps with the existing `SKIPPED` FunctionalSample status; scope to the functional/FuncSamplesReader path.
- **Rationale**: `SKIPPED` already exists (`functional.py:166`); `FuncSamplesReader` keeps SKIPPED (`:359-365`) while `LoadSamplesReader` drops it by design (`:264-265`). Customer use case is BBT/Selenium (functional).
- **Alternatives considered**: new status value — rejected, unnecessary; load-mode support — deferred (KPI-skew concern).

## Open item (for follow-up Story)
- Validate apiritif.loadgen does NOT auto-abort remaining iterations on first-iteration failure. Run `apiritif.loadgen --iterations 2` with a raising iteration 1 and observe iteration 2. If it aborts, escalate the approach.
- Decide per-step vs per-iteration granularity for the skipped marker (`status_per_iteration`).
