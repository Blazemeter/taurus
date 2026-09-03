# Data Model — MOB-53697 (taurus, spike)

This spike introduces no new persisted data. The relevant existing entities:

## FunctionalSample (existing)
- Fields: `test_case`, `test_suite`, `status` (PASSED/FAILED/BROKEN/SKIPPED), `start_time`, `duration`, `error_msg`, `error_trace`, `extras`, `subsamples`
- Source: `bzt/modules/functional.py:68-92`, statuses at `:166`
- Relevance: a skipped step would be emitted with `status="SKIPPED"`.

## Research finding (spike entity)
- Fields: `question`, `answer`, `evidence` (file:line list), `decision` (USE/NEW/AMBIGUOUS), `residual_risk`
- Relevance: the deliverable artifacts of this spike.

## Follow-up Story (spike entity)
- Fields: `key`, `epic` (MOB-47570), `scope_summary`
- Relevance: AC-2 deliverable.

## Future config field (recommendation, NOT added by this ticket)
- `skip_on_failure: bool` — scenario-level flag, default false. Read by the apiritif executor; threaded into code generation. Authoritative source is `a.blazemeter.com` config surface.
