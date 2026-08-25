# Quickstart: EFT for Browser-Based Tests — Taurus Side (MOB-43135)

## Enable the feature

Add to a Taurus YAML config with the BlazeMeter reporter:

```yaml
modules:
  blazemeter:
    generate-failed-transactions: true   # default: false

execution:
  - executor: selenium
    scenario: my-browser-scenario
```

When enabled, after a Selenium/Apiritif run completes, Taurus writes
`failed_transactions.json` into the run's artifacts directory; it is uploaded to
BlazeMeter as part of the standard `artifacts.zip`.

## What you get

`failed_transactions.json` — a per-run artifact whose failures are split into
`errors` (general), `assertions` (failed "Assert X" actions, each with a name),
and `failedEmbeddedResources`, keyed by the existing ERRTYPE_* classification.
Schema: `contracts/failed_transactions.schema.json`.

## Run the tests

```bash
python -m nose2 -s tests/unit tests.unit.modules.test_eft -v
python -m nose2 -s tests/unit tests.unit.modules.blazemeter.test_blazemeter_eft -v
# full suite (must stay green):
python -m nose2 -s tests/unit -v
```

## Notes
- Default off — existing runs are unaffected unless the flag is set.
- Browser tests emit empty `responseBodies` and may have null `rc`.
- Final json schema is subject to Sparta (Taurus team) sign-off; this is the initial format.
