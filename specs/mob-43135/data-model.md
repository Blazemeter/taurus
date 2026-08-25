# Data Model: EFT Failed-Transactions Artifact (MOB-43135, taurus)

## Entity: FailedTransactionsArtifact (`failed_transactions.json`)

The per-run EFT output written to `engine.artifacts_dir`.

| Field | Type | Source | Notes |
|---|---|---|---|
| `reportInfo` | object | generated | `{sessionId, testId, timestamp, type:"FAILED_TRANSACTIONS"}` |
| `transactions` | array<Transaction> | FunctionalAggregator.cumulative_results | one entry per transaction that has ≥1 failure |

## Entity: Transaction

| Field | Type | Source | Notes |
|---|---|---|---|
| `label` | str | FunctionalSample.label / ResultsTree key | transaction name |
| `timestamp` | int/float or null | FunctionalSample.start_time | may be null |
| `duration` | float or null | FunctionalSample.duration | may be null |
| `errors` | array<ErrorItem> | items typed ERRTYPE_ERROR (0) | general errors |
| `assertions` | array<AssertionItem> | items typed ERRTYPE_ASSERT (1) | failed assertions |
| `failedEmbeddedResources` | array<ErrorItem> | items typed ERRTYPE_SUBSAMPLE (2) | failed sub-samples |

## Entity: ErrorItem (mirrors `error_item_skel`, aggregator.py:271-279)

| Field | Type | Source | Notes |
|---|---|---|---|
| `cnt` | int | aggregation count | count of this error |
| `msg` | str | FunctionalSample.error_msg | error message |
| `tag` | str or null | assertion name (assertions only) | null for general errors |
| `rc` | str or null | — | null for browser tests (FR-005) |
| `type` | int | classify_failure | 0/1/2 (ERRTYPE_*) |
| `urls` | object | — | {url: count}; may be empty for Selenium |
| `responseBodies` | array | — | empty for browser tests (FR-005) |

## Entity: AssertionItem (mirrors DatapointSerializer assertions split, blazemeter_reporter.py:695-717)

| Field | Type | Source | Notes |
|---|---|---|---|
| `name` | str | recovered or synthetic (`assert::<label>`) | never empty (FR-004) |
| `failureMessage` | str | FunctionalSample.error_msg | |
| `failures` | int | count | |

## Validation rules
- Every failure MUST map to exactly one of `errors` / `assertions` / `failedEmbeddedResources` (SC-001).
- Every assertion MUST carry a non-empty `name` (real when recoverable, synthetic otherwise) (SC-003).
- The artifact MUST be valid, parseable JSON even with zero failures (empty collections) (US1 scenario 3).

## State / lifecycle
- Built once, at `BlazeMeterUploader.post_process()`, only when the config flag is truthy.
- Written to disk once; consumed by the existing `__upload_artifacts` zip+upload path.
