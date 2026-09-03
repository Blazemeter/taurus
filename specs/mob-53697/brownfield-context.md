# Brownfield Context: taurus — MOB-53697

## Semantic Disambiguation

| Ticket Identifier | Codebase Symbol | Codebase Meaning | Decision | Evidence (file:line) |
|---|---|---|---|---|
| iteration | `load.iterations` | Number of times the full test (all steps) repeats | USE | bzt/modules/_apiritif/executor.py:141-147; bzt/engine/modules.py:184 (LOAD_FMT namedtuple) |
| skip | "SKIPPED" status value | Test result status indicating test was skipped (not executed) | USE | bzt/modules/functional.py:166, 217, 253, 290 (TEST_STATUSES constant) |
| step | Transaction or HTTPRequest within a test | Individual request/transaction in a scenario | USE | bzt/modules/_apiritif/generator.py:2025-2026 (test method calls via master test method); bzt/modules/functional.py:67-92 (FunctionalSample structure) |
| scenario | `Scenario` object / scenario dict in config | Full test definition with all requests/transactions | USE | bzt/engine/modules.py:233-248; bzt/modules/_apiritif/executor.py:89-102 (scenario retrieval and builder initialization) |
| label | `request.label` / `transaction.label` | Name of a transaction, request, or test case | USE | bzt/modules/_apiritif/generator.py:1893-1897, 2045 (label in test methods) |
| status | `sample.status` | Test result status: PASSED, FAILED, BROKEN, SKIPPED | USE | bzt/modules/functional.py:68-92 (FunctionalSample.status); bzt/modules/reporting.py:133, 141, 152 |
| transaction | `apiritif.transaction` / `apiritif.smart_transaction` | Context manager wrapping a group of steps | USE | bzt/modules/_apiritif/generator.py:2203-2211 (_gen_transaction method) |
| continue_on_failure | none | No existing feature; inverse of requested skip-on-failure | NEW | grep found no match in bzt/modules/_apiritif/ or bzt/modules/ |
| skip_on_failure | none | No existing feature in codebase | NEW | grep found no match |
| skipOnFailure | none | No existing feature in codebase | NEW | grep found no match |

## Code Search Coverage

| Search Target | Patterns Used | Files Matched | Files Read | Omitted Matches / Reason |
|---|---|---|---|---|
| iteration handling | `iterations`, `load.iterations`, `get_load()`, `get_raw_load()` | 4 files | 2 (executor.py, modules.py) | Others are test files or examples |
| skip behavior | `skip`, `SKIPPED`, `skip_on_failure`, `skipOnFailure` | 5 files | 2 (functional.py, reporting.py) | Generator and executor files have only log lines, not skip logic |
| transaction/step flow | `transaction`, `_gen_transaction`, `_gen_test_method`, `_gen_master_test_method` | 1 file | 1 (generator.py) | Comprehensive; transaction is context manager |
| failure handling | `try`, `except`, `failure`, `abort`, `continue` | Multiple | 3 (generator.py, executor.py, functional.py) | No existing "stop on error" or "abort iteration" found |
| status values | `TEST_STATUSES`, `status`, `PASSED`, `FAILED`, `BROKEN`, `SKIPPED` | 2 files | 2 (functional.py, reporting.py) | Complete coverage of status enum |

## Negative Constraints

- `continue_on_failure`: Not present; inverse of requested feature, would conflict semantically.
- `stop_on_error`: Not present in functional execution path (only log messages in generator warnings).
- Existing transaction try/except: Only wraps external handlers and action IDs (generator.py:1972-2018), not iteration control.

## Binding Decisions

```yaml
approved_identifiers:
  - name: skip_on_failure
    reason: Clear, matches ticket intent, no collision with existing identifiers
    source: Ticket requirement; Python convention (snake_case)

  - name: SKIPPED
    reason: Existing functional test status; matches FunctionalSample.status enum
    source: bzt/modules/functional.py:166 TEST_STATUSES constant

  - name: iteration
    reason: Established concept in load config (load.iterations); means one full run of all steps
    source: bzt/modules/_apiritif/executor.py:141-147

ambiguous_identifiers:
  - name: status_per_iteration
    reason: Functional samples are per-test-case (method call), not explicitly per-iteration. Must clarify whether we track status per-step or per-iteration completion.
    evidence: bzt/modules/functional.py:67-92 (FunctionalSample is per test case, not per iteration loop)

forbidden_identifiers: []
```

## Runtime Data Availability Proof

> This spike ships no code, so there is no runtime read to plan. The table below documents, for each status/result data source, the full lifecycle (written / updated / deleted / expired / planned read) so the follow-up Story can reason about availability. For this spike every source is `available` at the point a future implementation would use it.

| Runtime Data | Written | Updated | Deleted / Expired | Planned Read | Available at Read Time? | Evidence (file:line) |
|---|---|---|---|---|---|---|
| Per-test-case status (`FunctionalSample.status`) | written when a test method/transaction completes | not updated after write (immutable sample) | not deleted / does not expire during a run (in-memory + `.ldjson` on disk) | planned read: future skip logic sets/reads status to mark `SKIPPED` | Yes — available | bzt/modules/functional.py:68-92; bzt/modules/functional.py:166 |
| JSON-LD result records (`*.ldjson`) | written per test case/transaction by apiritif.loadgen | appended as the run proceeds (never rewritten) | file persists for the run; not deleted/expired mid-run | planned read: `FuncSamplesReader` tails and parses it | Yes — available | bzt/modules/_apiritif/executor.py:184-186; bzt/modules/functional.py:324-365 |
| Aggregated per-label results (`ResultsTree`) | written as reporters consume samples | updated as more samples arrive | not deleted / does not expire during the run | planned read: reporting / Request Stats | Yes — available | bzt/modules/functional.py:112-127 |
| Iteration count (`load.iterations`) | written at config parse | not updated at runtime | not deleted / does not expire | planned read: passed as `--iterations` CLI arg to apiritif.loadgen | Yes — available | bzt/modules/_apiritif/executor.py:192-193 |
| SKIPPED status in report | written when a step is marked skipped | not updated | not deleted / does not expire | planned read: `FuncSamplesReader` keeps it; `LoadSamplesReader` drops it by design | Yes — available (functional path) | bzt/modules/functional.py:264-265, 359-365 |

## Cross-Repo Capability Analysis

| Candidate Service/API | Capability | Evidence | Decision | Rationale |
|---|---|---|---|---|
| taurus (local executor) | Read skip-on-failure flag from YAML scenario config; pass to apiritif via generator | bzt/modules/_apiritif/executor.py:186-207 (cmdline construction); bzt/modules/_apiritif/generator.py:156-182 (generator init from scenario) | USE | Taurus controls scenario parsing, code generation, and executor process invocation. Skip logic must live here (in generated code). |
| apiritif (Python test runner) | Execute test iterations with skip-rest-on-failure logic within iteration loop | Not examined (external package); called via `python -m apiritif.loadgen --iterations N` | MAYBE | Taurus generates test code but does not execute iterations; apiritif.loadgen (external CLI) runs the iteration loop. Its abort-on-failure behavior must be validated (see Assumption Ledger A4). |
| taurus-cloud / BlazeMeter API | Configure skip-on-failure as scenario property; persist in test config | Not examined here; covered by those repos' spike slices | NOT APPLICABLE (this slice) | If the feature is cloud-native (user-facing toggle), the API populates skip-on-failure in the scenario YAML sent to taurus. That surface is owned by a.blazemeter.com. |

## Cross-Service Contract Verification

none — this spike selects no cross-service API to call. Contract verification for the future config surface is deferred to the `a.blazemeter.com` spike slice. The taurus→apiritif boundary is a same-process code-generation boundary, analysed in the Boundary Compatibility Analysis below.

## Field/Metric Provenance Matrix

none — this spike renders no new derived metric fields. Existing "skipped" count in reporting (bzt/modules/reporting.py:508) already consumes SKIPPED status samples.

## Assumption Ledger

| ID | Assumption / Claim | Type | Evidence For | Evidence Against / Unknowns | Risk | Validation / Challenge Required | Decision |
|---|---|---|---|---|---|---|---|
| A1 | "Iteration" = one full run of the test_* method (all steps in sequence) | Semantic | load.iterations CLI arg passed to apiritif.loadgen; each iteration calls setUp→test_*→tearDown (bzt/modules/_apiritif/executor.py:141-147) | Apiritif iteration semantics not examined (external); could mean per-concurrency-thread loop | low | Spike AC#1: confirm iteration semantics from apiritif.loadgen | LOW-RISK (documented; consistent with load config) |
| A2 | Skipping remaining steps means skipping subsequent method calls within the iteration | Behavioral | _gen_master_test_method (generator.py:2020-2046) calls step methods sequentially; no skip-on-failure loop exists today | Current code always executes all steps; no explicit iteration-level control found | low | Spike AC#2: identify where break/return logic would go | LOW-RISK (selected design A places skip in generated code) |
| A3 | Per-iteration status can be recorded as SKIPPED without breaking aggregation | Behavioral | Existing SKIPPED status in TEST_STATUSES; LoadSamplesReader filters SKIPPED (functional.py:264-265); FuncSamplesReader keeps it (functional.py:359-365) | Granularity per-step vs per-iteration is undecided (`status_per_iteration`) | medium | Spike AC#3: prove marking a step SKIPPED is representable in the functional path | VALIDATE (follow-up Story confirms functional-path granularity) |
| A4 | Skipping iteration is not skipping the whole test (apiritif does not auto-abort remaining iterations on first failure) | Semantic | load.iterations is per-execution; check() reads aggregated results with no early-stop (executor.py:250-252) | Apiritif iteration loop is external/opaque; if apiritif stops on first failure, the feature cannot work as stated | medium | Spike AC#4 / assumption-challenge task T010: run apiritif.loadgen with 2 iterations, raise in iteration 1, observe iteration 2 | VALIDATE (T010 falsification test before committing to design A) |

## Boundary Compatibility Analysis

> The one boundary this spike must reason about is the taurus **producer** (generated test code + `--iterations` CLI arg) → apiritif.loadgen **consumer** (the iteration-loop runner). It is a same-process code-generation/subprocess boundary, not a service response.

| Boundary | Producer / Source Shape | Consumer / Helper Expected Shape | Compatibility / Equivalence Decision | Adapter / Mapping | Evidence (file:line) |
|---|---|---|---|---|---|
| taurus → apiritif.loadgen | taurus **produces** generated unittest-style `test_*` methods (a Python script) plus a `--iterations N` CLI arg (`executor.py:186-207`) | apiritif.loadgen **consumer** expects a runnable test script and an iterations count; it **returns** JSON-LD sample records with a `status` field | compatible today for normal runs; **compatibility of the skip signal is UNKNOWN until A4 is validated** (does an exception in iteration 1 abort remaining iterations?) | mapping: skip logic embedded in the generated `_gen_master_test_method` body (design A). If A4 shows apiritif aborts, an adapter is needed (non-exception skip signal / apiritif flag) | bzt/modules/_apiritif/generator.py:2020-2046 (producer); bzt/modules/_apiritif/executor.py:186-207, 250-252 (consumer invocation + result read) |

Decision: proceed with design A (skip in generated code), but the skip-signal compatibility is `VALIDATE`, not `USE` — gated on the T010 falsification test. No silent `PROCEED` on the unknown.

## Design Alternatives Considered

| Option | What changes | Pros | Cons/Risks | Evidence | Decision |
|---|---|---|---|---|---|
| **A: Generator-level try/except-and-break** | Wrap all step method calls in try/except within test_* method body; on failure, set flag and break or return. | Purely generated code; no apiritif dependency change; cleanly maps to skip-on-failure semantics (fail one step → skip remaining in same iteration). | Must modify generator.py:2024-2043 (master test method). Requires way to signal apiritif to continue to next iteration (return vs exception). If exception, must mark samples as skipped. | bzt/modules/_apiritif/generator.py:2020-2046 (_gen_master_test_method sequentially calls step methods) | **SELECTED**: keep all logic in generated test code; use exception or flag to signal early stop (pending A4). |
| **B: Apiritif runtime flag** | Add `--skip-on-failure` CLI arg to apiritif.loadgen invocation; apiritif owns iteration loop and skip logic. | Decouples Taurus from skip details; apiritif maintainers implement iteration control. | Requires apiritif contract change; Taurus doesn't control iteration loop (black box); harder to debug/test; risks a fork (violates constitution II). | bzt/modules/_apiritif/executor.py:186-207 (cmdline construction); apiritif is external, not in this codebase. | **REJECTED**: external dependency + fork risk. |
| **C: Executor-level wrapper** | ApiritifNoseExecutor intercepts each iteration result; if failed, stops or skips next iteration. | Executor-level control; reusable across test runners. | Apiritif is a subprocess; Taurus only sees final aggregated results (not per-iteration), would need a new reporting pipe; wrong semantics (steps still ran). | bzt/modules/_apiritif/executor.py:250-252 (check() reads aggregated results); no per-iteration callback. | **REJECTED**: not feasible; results are aggregated post-execution. |

## Test Validity Strategy

- **Assumption-challenge / falsification (A4)**: run apiritif.loadgen with 2 iterations where iteration 1 raises; the test FAILS the rejected "apiritif aborts all iterations" hypothesis only if iteration 2 executes. This is the lifecycle/availability proof for the iteration loop and gates design A.
- **Future unit test (generator.py)**: when `skip_on_failure: true`, the generated `test_*` method must contain early-exit-on-failure wrapping of step calls; when absent (default), the generated code is unchanged (regression).
- **Future integration test (functional path)**: 3 steps, skip-on-failure on, step 2 fails → step 1 PASSED, step 2 FAILED, step 3 SKIPPED; step 3 must not inflate error rates (functional.py:264-265 / 359-365 behavior).
- **Mock-reality rule**: do NOT mock apiritif's iteration loop in the A4 challenge — the whole point is to observe real apiritif iteration behavior.

## Spike Answers (taurus's perspective on the 4 research questions)

### AC#1: How is load / iteration currently applied for functional/BBT tests?

**Answer**: Iterations are applied via `load.iterations` (a property of the load configuration) passed as a CLI argument to the external apiritif.loadgen subprocess.

**Evidence**:
- **Load definition**: `LOAD_FMT = namedtuple("LoadSpec", "concurrency throughput ramp_up hold iterations duration steps")` — bzt/engine/modules.py:184
- **Apiritif executor reads load**: `iterations = self.get_raw_load().iterations` (or calculates default) — bzt/modules/_apiritif/executor.py:141-147
- **Iterations passed to subprocess**: `cmdline += ['--iterations', str(load.iterations)]` — bzt/modules/_apiritif/executor.py:192-193
- **Invocation**: `python -m apiritif.loadgen --iterations N <test_script.py>` — bzt/modules/_apiritif/executor.py:186-207

**How iterations work**: Taurus does NOT execute the iteration loop itself. It generates Python test code (unittest-style test_* methods) and invokes apiritif.loadgen (an external Python package), passing `--iterations N`. Apiritif.loadgen owns the iteration loop: it instantiates a unittest TestCase, calls setUp→test_*→tearDown N times, and aggregates results into JSON-LD file(s).

**Per-iteration semantics**: One iteration = one complete execution of setUp() → test_<label>() → tearDown(). The test_<label>() method sequentially calls all step methods (transactions/requests). Apiritif loops over iterations and reports aggregated samples.

---

### AC#2: How can we skip the rest of the iteration if any step fails?

**Answer**: The skip logic must be implemented in the generated test code, because Taurus does not control the iteration loop (apiritif.loadgen does). The generated test_* method can catch exceptions from step methods and conditionally skip remaining steps.

**Evidence**:
- **Test method generation**: `_gen_master_test_method()` generates the main test_<label>() method — bzt/modules/_apiritif/generator.py:2020-2046
- **Step invocation in master test**: Lines 2024-2026 show sequential method calls: `main_body.append(ast.Expr(ast_call(func=ast_attr("self." + slave_name))))` for each step method.
- **Already uses try/finally for teardown**: Lines 2032-2041 wrap step calls in try/finally if teardown methods exist, proving try/except is already part of generated code structure.
- **Exception wrapping exists for action IDs**: `_gen_actionid_exception_wrapped_body()` (generator.py:1933-2018) wraps individual transaction calls in try/except to track action IDs and re-raise.

**Implementation approach**: Modify _gen_master_test_method() to wrap step method calls with early-exit-on-failure so that, when `skip_on_failure` is set, the first failing step short-circuits the remaining step calls (teardown still runs). Alternatively signal apiritif to skip remaining steps (requires apiritif cooperation — see A4).

---

### AC#3: Is per-iteration scenario/label status set? Can we mark "skipped"?

**Answer**: Partially. Taurus tracks status per test case (per step/transaction), not explicitly per iteration. However, the SKIPPED status already exists and is representable in the data model.

**Evidence**:
- **FunctionalSample status field**: `status: str - test status (PASSED / FAILED / BROKEN / SKIPPED)` — bzt/modules/functional.py:68-92
- **TEST_STATUSES constant**: `("PASSED", "FAILED", "BROKEN", "SKIPPED")` — bzt/modules/functional.py:166, 217
- **Status persisted in JSON-LD**: Each functional sample record in the .ldjson file includes a "status" field — bzt/modules/functional.py:149-165 (SAMPLE_KEYS)
- **Status-to-code mapping**: `"SKIPPED": "300"` — bzt/modules/functional.py:253, 290 (LoadSamplesReader.STATUS_TO_CODE)

**Caveat**: LoadSamplesReader (load mode) explicitly filters SKIPPED samples — bzt/modules/functional.py:264-265 (`if item["status"] == 'SKIPPED': return`). This is intentional (avoids inflating error counts). FuncSamplesReader (functional mode) does NOT filter SKIPPED; it passes them to reporters — bzt/modules/functional.py:359-365.

**Conclusion**: Yes, we can mark steps as SKIPPED per iteration. The status field exists, SKIPPED is a valid value, and functional reports accept it. Load-mode reporting ignores SKIPPED (by design). For functional/BBT tests, SKIPPED samples are visible in results.

---

### AC#4: Does skipping the iteration mean skipping the whole test?

**Answer**: No. Skipping remaining steps in one iteration does NOT skip subsequent iterations or the whole test.

**Evidence**:
- **Iteration loop is in apiritif.loadgen**: bzt/modules/_apiritif/executor.py:186-207 (subprocess invocation with --iterations N)
- **Each iteration is independent**: Apiritif's iteration loop runs the test_* method N times. If iteration 1 fails step 2, apiritif continues to iteration 2 (subject to A4 validation).
- **Taurus aggregates results after all iterations**: bzt/modules/_apiritif/executor.py:250-252 (check() reads aggregated results; no early-stop logic).

**Conclusion**: Skipping remaining steps in iteration 1 does not prevent iteration 2 from running. Iteration count/load parameters are controlled by apiritif, not affected by per-iteration skip logic. Residual risk: if apiritif auto-stops on first iteration failure, the feature cannot work — validated by assumption-challenge task T010.

---

## Current State

### Relevant files

- **bzt/modules/_apiritif/executor.py:138-207** — ApiritifNoseExecutor.get_load() and startup(); iteration is passed as CLI arg to apiritif.loadgen
- **bzt/modules/_apiritif/generator.py:2020-2046** — _gen_master_test_method(); sequentially calls step methods; currently no skip-on-failure logic
- **bzt/modules/_apiritif/generator.py:2183-2211** — _gen_transaction(); wraps steps in apiritif.transaction context manager, enabling per-step status tracking
- **bzt/modules/functional.py:67-92** — FunctionalSample class; status field supports PASSED, FAILED, BROKEN, SKIPPED
- **bzt/modules/functional.py:166, 217, 253, 290** — TEST_STATUSES and STATUS_TO_CODE constants; SKIPPED is valid
- **bzt/modules/functional.py:324-365** — FuncSamplesReader; reads .ldjson reports and yields FunctionalSample objects with status field intact

### Root cause / gap

**Missing feature**: When a step fails in a functional/BBT test iteration, Taurus/Apiritif continues (or the whole test_* method aborts), causing cascading failures and inflated error counts (Independent Health). No logic exists to skip *only the remaining steps of the current iteration* while continuing subsequent iterations. Adding it needs generator changes + a scenario-level `skip_on_failure` flag.

---

## Desired State Delta

| What | File | Change |
|---|---|---|
| YAML schema / scenario property | scenario config | Add optional `skip_on_failure: bool` property to scenario config |
| Generator logic | bzt/modules/_apiritif/generator.py | Modify _gen_master_test_method() to wrap step calls; on failure skip remaining calls; mark skipped steps SKIPPED |
| Executor integration | bzt/modules/_apiritif/executor.py | Pass skip-on-failure flag to generator; read from scenario config |
| Reporting (no change) | bzt/modules/reporting.py, bzt/modules/functional.py | SKIPPED already handled; FuncSamplesReader passes SKIPPED; reporting counts SKIPPED |

---

## Files the planner must read for spec.md

1. **bzt/modules/_apiritif/generator.py:2020-2046** — _gen_master_test_method; where skip logic goes
2. **bzt/modules/_apiritif/executor.py:138-207** — get_load() and startup(); load passing and CLI construction
3. **bzt/modules/functional.py:67-92** — FunctionalSample class; status field
4. **bzt/modules/functional.py:324-365** — FuncSamplesReader; how JSON-LD is parsed into samples

---

## Planner Blockers

- none — the residual apiritif.loadgen iteration-abort question is a documented open item (Assumption Ledger A4 + task T010) for the follow-up Story, not a blocker on the spike deliverable. The spike's job is to surface it, which it has.

---

## overall_finding

brownfield

This is a brownfield feature request. Taurus has the data model and execution infrastructure (SKIPPED status, per-step transaction recording, aggregation) to support skip-on-failure, but the logic to detect step failure and skip remaining steps in an iteration does NOT exist. The feature requires code-generation changes (generator.py) and executor configuration (executor.py to read the flag from scenario). No naming conflicts or semantic collisions detected. The apiritif.loadgen iteration-abort behavior is a boundary question (A4) to validate before implementation, but does not block the spike.
