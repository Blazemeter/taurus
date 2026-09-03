# Test / Verification Scenarios — MOB-53697 (taurus, spike)

> **Spike ticket — no executable unit tests ship.** This ticket ships no production code, so there is nothing to unit-test at runtime. The scenarios below are the human-verifiable acceptance checks for the RESEARCH deliverable. Each is a documentation/Jira gate. The `test_*` names below are the machine-referenceable identifiers for those documentation-gate checks (so the design contract can trace each requirement to a verification); they are review checklists, not runtime pytest functions.

## Verification scenarios

| ID | Type | Verification (documentation/Jira gate) | Named check | Covers |
|---|---|---|---|---|
| S1 | positive | `plan.md` Research Findings → Q1 cites `bzt/modules/_apiritif/executor.py:141-147` and `:186-207` | test_q1_answer_cited | T002 / FR-001 |
| S2 | positive | `plan.md` Research Findings → Q2 cites `bzt/modules/_apiritif/generator.py:2020-2046` and `:1933-2018` | test_q2_answer_cited | T003 / FR-002 |
| S3 | positive | `plan.md` Research Findings → Q3 cites `bzt/modules/functional.py:166`, `:264-265`, `:359-365` | test_q3_answer_cited | T004 / FR-003 |
| S4 | positive | `plan.md` Research Findings → Q4 cites `bzt/modules/_apiritif/executor.py:186-207` and `:250-252` | test_q4_answer_cited | T005 / FR-004 |
| S5 | negative | `plan.md` Failure Modes documents the apiritif.loadgen iteration-abort risk and the `--iterations 2` falsification step (challenges assumption A4) | test_apiritif_abort_risk_documented | T006, T010 / FR-005 |
| S6 | positive | A follow-up implementation Story exists against Epic MOB-47570 | test_followup_story_filed | T007 / FR-006 |
| S7 | positive | Consolidated findings are posted to MOB-53697 (Jira comment or linked doc) | test_findings_published | T008 / FR-007 |

## Negative / falsification coverage

- `test_apiritif_abort_risk_documented` (S5) is the falsification check: it fails the tempting-wrong conclusion "apiritif keeps running all iterations" unless the documented `--iterations 2` validation step (raise in iteration 1, observe iteration 2) is present. This is 1 of 7 scenarios (~14% negative) — appropriate for a documentation spike where 6 checks are existence/citation gates and 1 is an explicit falsification.

## Coverage map

| FR / AC | Scenario | Named check |
|---|---|---|
| FR-001 / AC-1 | S1 | test_q1_answer_cited |
| FR-002 / AC-1 | S2 | test_q2_answer_cited |
| FR-003 / AC-1 | S3 | test_q3_answer_cited |
| FR-004 / AC-1 | S4 | test_q4_answer_cited |
| FR-005 / AC-1 | S5 | test_apiritif_abort_risk_documented |
| FR-006 / AC-2 | S6 | test_followup_story_filed |
| FR-007 / AC-1 | S7 | test_findings_published |
