# Tasks: Spike — SKIP ON FAILURE research (taurus)

**Feature**: MOB-53697 (SPIKE / research)
**Input**: `specs/mob-53697/spec.md`, `plan.md`, `brownfield-context.md`

> **Spike ticket — documentation/research tasks only.** No production code tasks and no unit-test tasks: MOB-53697 ships no code. Every task produces or verifies a documented research finding. The recommended future implementation is captured in `plan.md` for a follow-up Story, not built here. The FR/AC → task mapping lives in the Requirement Traceability Matrix in `spec.md`; each task ID appears exactly once in this file.

## Phase 1: Setup

- [ ] T001 Confirm the research artifacts exist and are code-grounded in `specs/mob-53697/brownfield-context.md` (Spike Answers section) and `specs/mob-53697/research.md`

## Phase 3: User Story 1 — Research questions answered with code evidence (P1)

**Goal**: Each of the 4 questions has a documented, file:line-cited answer.
**Independent test**: An engineer can verify each answer by reading the cited `file:line` without running code.

- [ ] T002 [P] [US1] Document answer to Q1 (how load/iterations are applied), citing `bzt/modules/_apiritif/executor.py:141-147` and `:186-207`, in `specs/mob-53697/plan.md` Research Findings section — satisfies FR-001 / AC-1
- [ ] T003 [P] [US1] Document answer to Q2 (how to skip the rest of the iteration on failure), citing `bzt/modules/_apiritif/generator.py:2020-2046` and `:1933-2018`, in `specs/mob-53697/plan.md` Research Findings section — satisfies FR-002 / AC-1
- [ ] T004 [P] [US1] Document answer to Q3 (per-iteration status; mark SKIPPED), citing `bzt/modules/functional.py:166`, `:264-265`, `:359-365`, in `specs/mob-53697/plan.md` Research Findings section — satisfies FR-003 / AC-1
- [ ] T005 [P] [US1] Document answer to Q4 (skipping iteration is not skipping the test), citing `bzt/modules/_apiritif/executor.py:186-207` and `:250-252`, in `specs/mob-53697/plan.md` Research Findings section — satisfies FR-004 / AC-1
- [ ] T006 [US1] Document the residual apiritif.loadgen iteration-abort risk and the recommended `--iterations 2` validation step in `specs/mob-53697/plan.md` Residual Risk section — satisfies FR-005 / AC-1

## Phase 4: User Story 2 — Follow-up implementation Story filed (P1)

**Goal**: Findings converted into a scoped implementation Story.
**Independent test**: A follow-up Story exists against Epic MOB-47570.

- [ ] T007 [US2] File a follow-up implementation Story against Epic MOB-47570 capturing SKIP ON FAILURE scope (scenario-level `skip_on_failure` flag; generator skip logic in `_gen_master_test_method`; reuse `SKIPPED` for the functional path; validate apiritif iteration semantics) — satisfies FR-006 / AC-2

## Phase 5: Polish & Cross-Cutting

- [ ] T008 Post the consolidated spike findings (all 4 answers, recommended approach, residual risk) to MOB-53697 as a Jira comment or linked doc — satisfies FR-007 / AC-1
- [ ] T009 Cross-reference the taurus findings against the `a.blazemeter.com` (config surface) and `taurus-cloud` (cloud orchestration) spike slices to confirm the cross-repo boundary in `specs/mob-53697/plan.md` Cross-Repo Boundary section
- [ ] T010 Assumption-challenge / falsification task (for the follow-up Story, documented as the spike's recommended validation): run `python -m apiritif.loadgen --iterations 2 <script>` with a raising iteration 1 and record whether iteration 2 executes — this is the data-availability / lifecycle proof for assumption A4 (apiritif does not auto-abort remaining iterations). Documented in `specs/mob-53697/plan.md` Failure Modes / Residual Risk section — challenges assumption A4 from `brownfield-context.md` Assumption Ledger

## Dependencies

- Setup precedes all user-story work.
- The four US1 documentation tasks are independent of each other (marked `[P]`); all precede the US2 filing task and the polish tasks.
- The follow-up-Story filing depends on the US1 findings being documented.
- The Jira publish task depends on all US1 findings and the follow-up-Story filing being complete.
- The assumption-challenge task documents the recommended validation the follow-up Story runs before committing to the selected design.

## Implementation Strategy

MVP = User Story 1 documentation tasks: the four answered questions plus the residual-risk note. User Story 2 and polish convert findings into scoped work and publish them. No code, no unit tests — this is a research spike. The FR/AC-to-task mapping is authoritative in the `spec.md` Requirement Traceability Matrix.
