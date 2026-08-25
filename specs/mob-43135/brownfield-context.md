# Brownfield Context: taurus — MOB-43135

## Reference Implementation Trace

### Behavioral Portrait of the Capability

**Protocol & Lifecycle:**
- EFT (Exclude Failed Transactions) is a reporting feature that collects per-transaction error details during test execution and produces a structured output artifact (JSON) containing categorized errors.
- Currently implemented for JMeter-based tests; the feature reads error data from JMeter's TJL error logs and categorizes failures into error types (response-code errors, assertions, sub-sample failures).
- The artifact is generated *after* test completion and uploaded to BlazeMeter backend alongside KPI data and other artifacts.
- Authentication: inherits from BlazeMeter uploader (API token-based).

**IO Shape:**
- Input: JTL error file (XML format, read incrementally by `JTLErrorsReader`)
- Output: JSON artifact (structure pending Sparta sign-off; the initial format mirrors error_item_skel and must be compatible with a.blazemeter.com Master::isTransactionFilterAllowed gating logic)
- Data operated on: per-transaction error messages, response codes, assertion failures, error types (ERRTYPE_ERROR=0, ERRTYPE_ASSERT=1, ERRTYPE_SUBSAMPLE=2)

**Auth Needs:**
- Inherits from BlazeMeter uploader session token; no separate auth required

**Producer/Consumer:**
- Producer: taurus (this repo) — aggregates errors and generates JSON artifact
- Consumer: a.blazemeter.com backend (downstream) — reads JSON artifact during session post-processing; gates feature to JMETER + EXTERNAL_RESULT_LOADER executors only (line 3639 in Master.php)

**Result Destination:**
- Uploaded to BlazeMeter via `BlazeMeterUploader._session.upload_file()` during post_process phase (after KPI data)

### Candidate Analogs & Property Match

**Selected Analog:** existing jmx-based EFT JSON output (JMeter-side)

| Property | JMeter (Existing) | Selenium (New) | Evidence |
|----------|-------------------|----------------|----------|
| Data source | JTL error file (XML) | Apiritif/Selenium result files (LDJSON in functional mode) | `/bzt/modules/jmeter.py:1255` (JTLErrorsReader), `/bzt/modules/_apiritif/executor.py:184` (ldjson in functional) |
| Error categorization | ERRTYPE_ERROR, ERRTYPE_ASSERT, ERRTYPE_SUBSAMPLE | Must support same types + assertion categorization | `/bzt/modules/aggregator.py:225-227` (three error types), `/bzt/modules/jmeter.py:1413-1453` (find_failure logic) |
| Timestamp tracking | Per-sample timestamp from JTL | Per-sample timestamp from functional results | `/bzt/modules/jmeter.py:1346-1352` (extract standard/nonstandard) |
| Aggregation point | BlazeMeterUploader post_process | BlazeMeterUploader post_process (same) | `/bzt/modules/blazemeter/blazemeter_reporter.py:300-349` (post_process phase) |
| Upload mechanism | session.upload_file() to artifacts.zip | session.upload_file() to artifacts.zip (same) | `/bzt/modules/blazemeter/blazemeter_reporter.py:268-298` (__upload_artifacts) |

**Rejection of candidates:**
- CSV/LDJSON intermediate formats (apiritif functional output): acceptable for internal transaction tracking, but a.blazemeter.com expects a unified JSON schema for EFT filtering, so final artifact must be JSON with consistent schema across executor types
- InfluxDB reporter: reports aggregated metrics only, not individual transaction errors; cannot serve as EFT source

---

### Per-Layer Trace (taurus → a.blazemeter.com → UI)

#### taurus Layer (Executor → Aggregator → Uploader)

**Executor Registration & Transaction Capture:**
- SeleniumExecutor registers a runner (apiritif, junit, pytest, etc.) and subscribes to transaction lifecycle events
- Location: `/bzt/modules/_selenium.py:66-68` — `subscribe_to_transactions(listener)`
- ApiritifNoseExecutor parses stdout for "Transaction started" / "Transaction ended" markers
- Location: `/bzt/modules/_apiritif/executor.py:231-248`
- Runner emits transaction_started(label, start_time) and transacion_ended(label, duration) callbacks
- Location: `/bzt/modules/__init__.py:102-126` (ReportableExecutor base class)

**Error Data Flow During Execution:**
- JMeter: JTLErrorsReader parses errors.jtl XML incrementally, extracts (label, message, rc, error_type, timestamp)
- Location: `/bzt/modules/jmeter.py:1317-1454` (JTLErrorsReader._parse_element → find_failure)
- Selenium/Apiritif: FunctionalAggregator accumulates FunctionalSample objects with status (PASSED/FAILED/BROKEN) and error_msg/error_trace
- Location: `/bzt/modules/functional.py:67-128` (FunctionalSample, ResultsTree, FunctionalAggregator)
- KPISet error items (error_item_skel) built during aggregation; errors list contains dicts with keys: cnt, msg, tag, rc, type (ERRTYPE_*), urls, responseBodies
- Location: `/bzt/modules/aggregator.py:266-279` (error_item_skel signature and structure)

**BlazeMeter Uploader Integration:**
- BlazeMeterUploader.post_process() calls __upload_artifacts()
- All files in engine.artifacts_dir are zipped and uploaded to BlazeMeter session
- Location: `/bzt/modules/blazemeter/blazemeter_reporter.py:268-298` (__upload_artifacts)
- EFT artifact file name (initial choice): "failed_transactions.json", placed in engine.artifacts_dir (subject to Sparta confirmation)
- Uploader has no explicit EFT handling yet (JMeter was probably handled upstream in JMeter module itself)

**Open Question — Where is JMeter EFT JSON currently generated?**
- Search in jmeter.py and blazemeter_reporter.py for "json" output related to errors → no explicit EFT JSON generation found in taurus
- Possible: EFT JSON is generated *upstream* by JMeter itself or by an external tool, not by taurus
- Or: EFT filtering is handled *downstream* by a.blazemeter.com after receiving raw error data

#### a.blazemeter.com Layer (EFT Gating)

**Transaction Filter Gating:**
- Master::isTransactionFilterAllowed() — determines whether a test can use EFT feature
- Checks: account has EXCLUDE_FAILED_TRANSACTIONS feature flag, test has Taurus executions, all executors are in [SCRIPT_TYPE_JMETER, SCRIPT_TYPE_EXTERNAL_RESULT_LOADER]
- Location: `/home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Model/Master.php:3629-3644`
- **Current limitation:** blocks SCRIPT_TYPE_SELENIUM and other executors (line 3639 requires all executors to be JMETER or EXTERNAL_RESULT_LOADER)
- This gate must be updated to allow SCRIPT_TYPE_SELENIUM once taurus generates valid EFT JSON

**Executor Type Constants:**
- Location: `/home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Helpers/TaurusConfiguration/TaurusExecution.php:53-57`
- SCRIPT_TYPE_JMETER = 'jmeter'
- SCRIPT_TYPE_SELENIUM = 'selenium'
- SCRIPT_TYPE_EXTERNAL_RESULT_LOADER = 'external-results-loader'
- SCRIPT_TYPE_K6 = 'k6'

**Data Ingestion (not yet located in a.blazemeter.com source):**
- External results import route exists: `/tests/{testId}/external-results-import` (line 576 in Tests.php)
- Currently marked @deprecated; actual handler for EFT JSON ingestion not yet located in browsed code
- Likely resides in worker or service layer downstream; must be verified during planning phase

#### UI Layer (v3)

- Not in scope for this research; v3 consumes Master::isTransactionFilterAllowed() boolean to enable/disable EFT UI controls

---

### JSON Shape & Schema Invariants

**Existing JMeter Error Structure (from KPISet):**
```python
# From /bzt/modules/aggregator.py:271-279 (error_item_skel)
{
    "cnt": int,                # count of this error
    "msg": str,                # error message
    "tag": str or None,        # assertion name (if ERRTYPE_ASSERT), else None
    "rc": str or None,         # response code (if applicable), else None
    "type": int,               # 0=ERRTYPE_ERROR, 1=ERRTYPE_ASSERT, 2=ERRTYPE_SUBSAMPLE
    "urls": Counter,           # {url: count, ...} of failed URLs (subsample or initial request)
    "responseBodies": [        # error response body samples (for debugging)
        {
            "content": str,
            "type": str,       # e.g. 'text/html; charset=UTF-8'
            "original_size": int,
            "hash": int,
            "cnt": int
        }
    ]
}
```

**Blaze Meter Uploader KPI Payload (error categorization into JSON):**
- Location: `/bzt/modules/blazemeter/blazemeter_reporter.py:695-717` (DatapointSerializer.__add_errors)
- Errors split by type: ERRTYPE_ERROR → 'errors', ERRTYPE_ASSERT → 'assertions', ERRTYPE_SUBSAMPLE → 'failedEmbeddedResources'
- JSON fragment:
```json
{
  "errors": [
    {"m": "message", "rc": "500", "count": 1},
    ...
  ],
  "assertions": [
    {"failureMessage": "assertion msg", "name": "assert_name", "failures": 1},
    ...
  ],
  "failedEmbeddedResources": [
    {"count": 1, "rm": "msg", "rc": "404", "url": "http://..."},
    ...
  ]
}
```

**EFT JSON Artifact (to be designed):**
- Candidate structure (matching DatapointSerializer split by type):
```json
{
  "reportInfo": {
    "sessionId": "...",
    "testId": "...",
    "timestamp": 1234567890,
    "type": "FAILED_TRANSACTIONS"
  },
  "transactions": [
    {
      "label": "transaction_name",
      "timestamp": 1234567890,
      "duration": 0.123,
      "errors": [
        {"msg": "...", "rc": "500", "type": 0, "count": 1},
        ...
      ],
      "assertions": [
        {"msg": "...", "name": "assert_name", "type": 1, "count": 1},
        ...
      ],
      "subsamples": [
        {"msg": "...", "rc": "404", "type": 2, "count": 1, "url": "..."},
        ...
      ]
    },
    ...
  ]
}
```
- **Final shape pending Sparta review** — must match a.blazemeter.com ingestion schema

---

## Semantic Disambiguation

| Ticket Identifier | Codebase Symbol | Codebase Meaning | Decision | Evidence |
|---|---|---|---|---|
| "exclude failed transactions" / EFT | `Master::isTransactionFilterAllowed()` | Boolean gate in a.blazemeter.com backend; controls whether transaction-level error filtering UI feature is enabled | USE | `/home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Model/Master.php:3629` |
| "exclude failed transactions" / EFT | No direct taurus symbol found | Feature does not yet exist in taurus; must be implemented as new JSON artifact generation + upload | NEW | grep -r "exclude.*fail\|eft" in taurus yields only BlazeMeter reporter imports, not EFT-specific code |
| "failed transaction" | KPISet.ERRORS list | Per-label aggregated error items, each with msg, rc, type, count | USE | `/bzt/modules/aggregator.py:211-247` (KPISet.__init__ defines ERRORS key) |
| "failed assertion" | KPISet.ERRTYPE_ASSERT = 1 | Assertion failures distinguished from HTTP response errors via error type enum | USE | `/bzt/modules/aggregator.py:226` (ERRTYPE_ASSERT constant) |
| "assert" / "assertion actions" | ApiritifScriptGenerator._gen_sel_assertion, _gen_assert_store_mngr | Methods that generate assertion code in apiritif-generated test scripts; assertions invoke self.assertEqual, self.assertTrue, etc. | USE | `/bzt/modules/_apiritif/generator.py:504-556` (assertion generation) |
| "error categorization" | error_item_skel(error_type) | Categorizes errors into three types via int enum: ERRTYPE_ERROR (response code), ERRTYPE_ASSERT (test assertion), ERRTYPE_SUBSAMPLE (embedded resource) | USE | `/bzt/modules/aggregator.py:266-279` (error_item_skel signature) |
| "json artifact name/format" | No taurus constant found | Taurus does not yet define EFT JSON artifact name or format | NEW | Will be determined during planning phase in consultation with Sparta |
| "transaction" | FunctionalSample.test_case, FunctionalSample.test_suite | In functional test context, transactions are individual test cases/methods; transaction lifecycle tracked via transaction_started/transaction_ended callbacks | USE | `/bzt/modules/functional.py:67-110` (FunctionalSample) |
| "transaction" | JMX transaction controller / <sample> element in JTL | In JMeter context, transaction is a transaction controller wrapping multiple requests; error data extracted from <sample> elements | USE | `/bzt/modules/jmeter.py:1255-1454` (JTLErrorsReader parses <sample>/<httpSample> hierarchy) |
| "executor" / "reporter naming" | SCRIPT_TYPE_JMETER, SCRIPT_TYPE_SELENIUM, etc. (in a.blazemeter.com) | Enumeration of executor types recognized by Taurus config in backend | USE | `/home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Helpers/TaurusConfiguration/TaurusExecution.php:40-57` |
| "executor" / "reporting" (taurus-side) | ScenarioExecutor, ReportableExecutor, SubprocessedExecutor (base classes) | Taurus executor class hierarchy; ReportableExecutor adds result-reporting capability | USE | `/bzt/modules/__init__.py:102-126` (ReportableExecutor with transaction subscription) |

---

## Code Search Coverage

| Search Target | Patterns Used | Files Matched | Files Read | Omitted Matches / Reason |
|---|---|---|---|---|
| EFT implementation in taurus | `exclude.*fail`, `eft`, `EFT` (case-insensitive) | 15 files (jmx2yaml, jmx/base, jmx/tools, utils, console, reporting, screen, blazemeter_reporter, apiritif, etc.) | blazemeter_reporter.py, jmeter.py, aggregator.py, functional.py, _selenium.py, _apiritif/executor.py, _apiritif/generator.py | Most matches were false positives (jmx2yaml, jmx/*) or unrelated context (e.g., "left"/"right" merging in reporter); no EFT JSON generation found |
| JTL error reading & categorization | `JTLErrorsReader`, `error.*type`, `ERRTYPE_`, `find_failure` | 2 files (jmeter.py, test files) | jmeter.py (lines 1255-1454) | Comprehensive trace of error extraction and type classification |
| Functional result readers (Selenium) | `FunctionalResultsReader`, `FunctionalSample`, `functional.*Selenium` | 5 files (functional.py, test files, _apiritif/executor.py, _pytest.py, etc.) | functional.py (lines 67-145), _apiritif/executor.py (lines 52-60, 231-248) | Located transaction lifecycle tracking and functional sample model |
| BlazeMeter uploader artifact handling | `upload_file`, `post_process`, `__upload_artifacts` | 1 file (blazemeter_reporter.py) | blazemeter_reporter.py (lines 268-298, 300-349) | Comprehensive trace of artifact upload mechanism |
| Executor type checking in a.blazemeter.com | `isTransactionFilterAllowed`, `SCRIPT_TYPE_` | 10 files (Master.php, TaurusExecution.php, Controllers, Migrations, etc.) | Master.php (lines 3629-3644), TaurusExecution.php (lines 40-57) | Located gating logic and executor type constants; confirmed SELENIUM not yet in allowed list |
| Error structure & aggregation | `error_item_skel`, `inc_list`, `KPISet.ERRORS` | 6 files (aggregator.py, jmeter.py, blazemeter_reporter.py, etc.) | aggregator.py (lines 206-379), blazemeter_reporter.py (lines 695-717) | Comprehensive understanding of error item structure and payload serialization |
| Assertion handling in Selenium | `_gen_assert`, `_gen_sel_assertion`, `assert` | 3 files (_apiritif/generator.py, test files) | _apiritif/generator.py (lines 504-556, 2293-2303, 2385+) | Located assertion code generation; assertion failures classified as ERRTYPE_ASSERT |
| Transaction tracking (apiritif) | `transaction_started`, `transacion_ended`, `subscribe_to_transactions` | 3 files (_apiritif/executor.py, __init__.py, _selenium.py) | __init__.py (lines 102-126), _apiritif/executor.py (lines 231-248), _selenium.py (lines 35-68) | Comprehensive trace of transaction event lifecycle |

---

## Negative Constraints

- **DO NOT** hard-code EFT JSON schema in taurus until Sparta team provides final format approval; design for extensibility via config-driven schema mapping
- **DO NOT** modify JTLErrorsReader to produce taurus-specific output; keep it JMeter-specific and extract/reuse only the error categorization logic (ERRTYPE_* enums and type classification)
- **DO NOT** upload EFT JSON separately from artifacts.zip; include it as a file within the zip (e.g., `artifacts/failed_transactions.json`)
- **DO NOT** gate EFT feature in taurus itself; gating lives in a.blazemeter.com (Master::isTransactionFilterAllowed); taurus must generate the artifact unconditionally if EFT option is set in config
- **DO NOT** assume Selenium error messages will have HTTP response codes (rc may be null); handle gracefully in JSON output

---

## Binding Decisions

```yaml
approved_identifiers:
  - name: "KPISet.ERRTYPE_ERROR"
    reason: "Existing enum for non-assertion, non-subsample errors (response codes, HTTP failures); reuse for Selenium"
    source: "JIRA:MOB-43135, /bzt/modules/aggregator.py:225"
  - name: "KPISet.ERRTYPE_ASSERT"
    reason: "Existing enum for assertion failures; reuse to categorize Selenium/Apiritif assertion failures"
    source: "JIRA:MOB-43135, /bzt/modules/aggregator.py:226"
  - name: "KPISet.ERRTYPE_SUBSAMPLE"
    reason: "Existing enum for failed sub-samples (embedded resources); may apply to failed Selenium sub-transactions (e.g., failed assertion within transaction step)"
    source: "JIRA:MOB-43135, /bzt/modules/aggregator.py:227"
  - name: "FunctionalSample + ResultsTree (from functional.py)"
    reason: "Existing data structure for functional test results (Selenium/Apiritif); source of truth for per-transaction pass/fail and error details"
    source: "JIRA:MOB-43135, /bzt/modules/functional.py:67-128"
  - name: "BlazeMeterUploader.post_process() artifact upload mechanism"
    reason: "Existing upload path for all artifacts; EFT JSON should follow the same mechanism (file in artifacts_dir, zipped, uploaded)"
    source: "JIRA:MOB-43135, /bzt/modules/blazemeter/blazemeter_reporter.py:268-298"
  - name: "SCRIPT_TYPE_SELENIUM constant (a.blazemeter.com)"
    reason: "Backend already recognizes 'selenium' as executor type; must update Master::isTransactionFilterAllowed() to include it in allowed list"
    source: "JIRA:MOB-43135, /home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Helpers/TaurusConfiguration/TaurusExecution.php:55"
  - name: "generate-failed-transactions (new Taurus config flag)"
    reason: "EFT json generation is opt-in via a new Taurus config flag under modules.blazemeter (generate-failed-transactions), default off - RESOLVED via Slack by U05EWFCHS8M (option A) on message 1787657070.534619; mirrors existing per-module reporter opts and gives the safest rollout"
    source: "SLACK:1787646729.715029:1787657070.534619"

forbidden_identifiers: []

# ambiguous_identifiers: all three resolved before /specify - see resolved_ambiguities below.
ambiguous_identifiers: []

resolved_ambiguities:
  - name: "Whether EFT filtering is opt-in or always-on"
    resolution: "RESOLVED via Slack (option A) - opt-in via new Taurus config flag modules.blazemeter.generate-failed-transactions, default off. Taurus generates the artifact only when the flag is set; server-side gating (Master::isTransactionFilterAllowed) remains an independent availability control."
    source: "SLACK:1787646729.715029:1787657070.534619"
  - name: "EFT JSON artifact file location in taurus"
    resolution: "RESOLVED confidence-first - write to engine.artifacts_dir/failed_transactions.json (bundled into artifacts.zip by the existing __upload_artifacts path, per Negative Constraint about not uploading EFT JSON separately). This is the initial format; final file name/schema is subject to Sparta approval (tracked as a VALIDATE assumption + dependency task, not a blocker - reviewer handoff already accepts Sparta approval as non-blocking)."
    source: "HANDOFF:task-reviewer:outputs.data.dependencies[0]; FILE:/bzt/modules/blazemeter/blazemeter_reporter.py:268-298"
  - name: "Error categorization strategy for Selenium assertion failures"
    resolution: "RESOLVED confidence-first as a VALIDATE assumption (A-ASSERT). Reuse the existing ERRTYPE_ERROR / ERRTYPE_ASSERT / ERRTYPE_SUBSAMPLE enum classification (aggregator.py:225-227). Assertion-name extraction from FunctionalSample.error_msg/error_trace is unproven at the source level, so plan.md/tasks.md MUST include a contract-verification/POC task that reads a real Selenium/Apiritif assertion-failure fixture and proves the assertion name is recoverable; if not recoverable, degrade to a synthetic name derived from the transaction label rather than blocking. Not a planner blocker - it is an explicit validation task."
    source: "FILE:/bzt/modules/aggregator.py:225-227; FILE:/bzt/modules/functional.py:67-110"
```

---

## Runtime Data Availability Proof

| Runtime Data | Written | Updated | Deleted/Expired | Planned Read | Available at Read Time? | Evidence | Safer Alternative if Unavailable |
|---|---|---|---|---|---|---|
| Per-transaction error list (JMeter) | JTL error file (incremental write by JMeter) | Read continuously by JTLErrorsReader during test | Deleted at test cleanup | Post-test (post_process phase) when BlazeMeterUploader calls __upload_artifacts() | **yes** — JTL file exists until cleanup | `/bzt/modules/jmeter.py:1283-1315` (read_file loop), `/bzt/modules/blazemeter/blazemeter_reporter.py:268-298` (called in post_process) | JMeter process output (stdout/stderr) could be parsed as fallback, but not recommended |
| Per-transaction error list (Selenium/Apiritif) | LDJSON result file or functional aggregator memory | Accumulates during test execution | Expires at engine shutdown | Post-test (post_process phase) when FunctionalAggregator.post_process() finalizes | **yes** — FunctionalAggregator.cumulative_results (ResultsTree) held in memory; LDJSON files on disk | `/bzt/modules/functional.py:47-64` (FunctionalAggregator.process_readers called in check and post_process), `/bzt/modules/_apiritif/executor.py:184` (ldjson file created) | FunctionalSample objects in cumulative_results are the authoritative source |
| Transaction labels & start/end times | Memory (Python objects: FunctionalSample, transaction_started/ended callbacks) | Read-only after recording | Never explicitly deleted; garbage-collected after post_process | Post-test via FunctionalAggregator.cumulative_results | **yes** — FunctionalAggregator holds ResultsTree until post_process completes | `/bzt/modules/functional.py:112-128` (ResultsTree.add_sample), `/bzt/modules/__init__.py:122-126` (transaction callbacks) | Store transaction metadata in LDJSON before finalization (already done by apiritif loadgen) |
| Assertion failure details (name, message, stacktrace) | FunctionalSample.error_msg, error_trace (if failed) | Read-only after test case completion | Garbage-collected after post_process | Post-test in __upload_artifacts() via aggregator snapshot | **maybe** — error_msg is a single string, not structured; assertion *name* may be lost if only traceback is stored | `/bzt/modules/functional.py:67-110` (FunctionalSample fields); `/bzt/modules/jmeter.py:1413-1453` (find_failure extracts assertion name from XML tag) | **RISK:** Apiritif/Selenium frameworks may not expose assertion name separately; may need to parse from error_trace string or generate synthetic name |
| Error response bodies (for debugging) | JTLErrorsReader._collected_error_responses (if enabled) | Collected incrementally during JTL read | Expires at engine shutdown | Post-test via KPISet.ERRORS[*].responseBodies | **yes** — KPISet error items contain responseBodies list | `/bzt/modules/jmeter.py:1399-1403` (collect_error_response_bodies mechanism), `/bzt/modules/aggregator.py:282-292` (_get_response_bodies) | May not be supported for Selenium; degrade gracefully to empty responseBodies |

**Summary:** All critical runtime data is available at the post_process read point for both JMeter and Selenium. **Risk area:** assertion failure names may not be reliably extractable from Selenium/Apiritif error traces without additional instrumentation.

---

## Cross-Repo Capability Analysis

| Candidate Service/API | Capability | Evidence | Decision | Rationale |
|---|---|---|---|---|
| a.blazemeter.com Master::isTransactionFilterAllowed() | Gating logic to enable/disable EFT for tests based on executor type and account feature flag | `/home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Model/Master.php:3629-3644` | USE | Gate must be updated to include SCRIPT_TYPE_SELENIUM in allowed executor list (line 3639) once taurus generates valid EFT JSON |
| a.blazemeter.com external-results-import endpoint | Ingestion path for EFT JSON (or other external result formats) | `/home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Routing/v4/Tests.php:576` | MAYBE | Route exists but marked @deprecated; actual handler logic and schema not yet located in browsed code; must verify during planning whether this is the target ingestion point or if a new endpoint is needed |
| BlazeMeterUploader artifact upload mechanism | Generic file upload to session artifacts via __upload_artifacts() and __get_jtls_and_more() | `/bzt/modules/blazemeter/blazemeter_reporter.py:268-298` | USE | EFT JSON should be placed in engine.artifacts_dir and included in the standard artifacts.zip upload; no new upload logic required |
| Taurus-cloud Docker image + S3 upload | Cloud execution wrapper; may need EFT JSON artifact handling if functional tests run in cloud | `/home/jenkins/blazemeter/taurus-cloud/` (not yet examined) | NOT APPLICABLE | Ticket scope is taurus-side generation + a.blazemeter.com ingestion; cloud execution is downstream and should inherit EFT handling automatically |
| Sparta (Taurus team) JSON format specification | Final format/schema for EFT JSON artifact | `/home/jenkins/blazemeter/a.blazemeter.com/src/` (no implementation yet) | NEEDS_APPROVAL | **Dependency:** Sparta team owns format approval; taurus must implement to their spec, not independently design |

---

## Cross-Service Contract Verification

### Contract: taurus → a.blazemeter.com (EFT JSON Upload & Ingestion)

**Route/Method/Path:**
- Taurus: `BlazeMeterUploader._session.upload_file(filename, content)` (wrapper around BlazeMeter API file upload)
  - Evidence: `/bzt/modules/blazemeter/blazemeter_reporter.py:283` (artifacts.zip upload), line 294 (log file upload)
  - Files uploaded to session artifacts via HTTP POST to BlazeMeter backend

**Request Schema (pending Sparta sign-off):**
- File format: JSON (candidate name: "failed_transactions.json" or Sparta-specified)
- Content: array of transaction error objects with label, timestamp, error lists, assertion details, subsample failures
- Encoding: UTF-8, gzipped within artifacts.zip

**Response Schema:**
- BlazeMeter API: HTTP 200 OK (file accepted) or error status
- No specialized EFT response; standard file upload acknowledgment

**Client/Auth/Timeout/Fallback:**
- Auth: inherited from BlazeMeterUploader session token (set at prepare time)
  - Evidence: `/bzt/modules/blazemeter/blazemeter_reporter.py:55-88` (init and prepare methods set session token)
- Timeout: inherited from user.timeout setting (default per bza.User)
  - Evidence: `/bzt/modules/blazemeter/blazemeter_reporter.py:120` (self._user.timeout)
- Fallback: if upload_file fails, logged as warning; test continues (graceful degradation)
  - Evidence: `/bzt/modules/blazemeter/blazemeter_reporter.py:333-338` (_postproc_phase2 has try/except around __upload_artifacts)

**Tests/Fixtures:**
- JMeter artifact upload tests: `/bzt/tests/unit/modules/test_blazeMeterUploader.py` (search for upload_file calls)
- Must extend with Selenium/apiritif EFT JSON upload tests (added in tasks.md as test_blazemeter_eft.py)

---

## Dependency Contract Facts: a.blazemeter.com (EFT Ingestion & Gating)

### Master::isTransactionFilterAllowed() Gate

**Endpoint:** Backend method (not a route); called during session startup by CollectionController
- Evidence: `/home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Controller/CollectionController.php:485, 661`

**Current Field List & Logic:**
- Input: Master object (contains: account, executions array with executor type)
- Checks:
  1. Account has EXCLUDE_FAILED_TRANSACTIONS feature flag
  2. Master has Taurus executions (non-empty)
  3. ALL executors in execution list are in: [SCRIPT_TYPE_JMETER, SCRIPT_TYPE_EXTERNAL_RESULT_LOADER]
- Output: bool (true if all checks pass)

**Fields Present/Absent:**
- Present: SCRIPT_TYPE_JMETER, SCRIPT_TYPE_EXTERNAL_RESULT_LOADER
- Absent: SCRIPT_TYPE_SELENIUM (must be added to allowed list)

**Update Required:**
- Line 3639 in Master.php: change
```php
$diff = array_diff($executors, [TaurusExecution::SCRIPT_TYPE_JMETER, TaurusExecution::SCRIPT_TYPE_EXTERNAL_RESULT_LOADER]);
```
to
```php
$diff = array_diff($executors, [TaurusExecution::SCRIPT_TYPE_JMETER, TaurusExecution::SCRIPT_TYPE_EXTERNAL_RESULT_LOADER, TaurusExecution::SCRIPT_TYPE_SELENIUM]);
```

### EFT JSON Ingestion Handler (not yet located in a.blazemeter.com source)

**Status:** Not yet located in a.blazemeter.com source during this research
- Likely resides in external-results-import endpoint handler or a worker that processes uploaded artifacts
- Must be verified in planning phase; may need new endpoint/handler if external-results-import is not the target

---

## Field/Metric Provenance Matrix

| Output Field | Derived From | Source Evidence | Transformation |
|---|---|---|---|
| transaction.label | FunctionalSample.test_suite + test_case (or explicit path) | `/bzt/modules/functional.py:94-104` (get_fqn, get_short_name) | Concatenate or use path array |
| transaction.timestamp | FunctionalSample.start_time | `/bzt/modules/functional.py:86` (start_time field) | Epoch seconds; no transform |
| transaction.duration | FunctionalSample.duration | `/bzt/modules/functional.py:87` (duration field) | In seconds; no transform |
| error.msg | FunctionalSample.error_msg (on failure) or KPISet error item msg | `/bzt/modules/functional.py:88` (error_msg), `/bzt/modules/aggregator.py:273` (error_item_skel msg) | Direct; no transform |
| error.type | FunctionalSample status (FAILED/BROKEN) categorized by assertion detection | `/bzt/modules/functional.py:85` (status field), `/bzt/modules/jmeter.py:1425-1432` (assertion detection logic) | Parse error_trace or error_msg to detect assertion; map to ERRTYPE_ASSERT or ERRTYPE_ERROR |
| error.rc | Response code (if HTTP-level error) or null (if assertion/browser error) | `/bzt/modules/aggregator.py:275` (error_item_skel rc), `/bzt/modules/jmeter.py:1349-1350` (extract rc) | Direct for HTTP errors; null for assertion failures |
| assertion.name | Assertion name (if ERRTYPE_ASSERT) extracted from assertion metadata or error trace | `/bzt/modules/jmeter.py:1425` (parse_assertion name), `/bzt/modules/_apiritif/generator.py:509-527` (assertion code with self.assertEqual, etc.) | Parse assertion name from test framework output or synthetic (e.g., "AssertionError on line X") |
| subsample.url | Failed subsample URL (if applicable) | `/bzt/modules/jmeter.py:1380-1397` (_get_failed_subsample_urls), `/bzt/modules/aggregator.py:277` (error_item_skel urls) | Direct from Counter; may be empty for non-HTTP errors |

---

## Assumption Ledger

| ID | Assumption/Claim | Type | Evidence For | Evidence Against/Unknowns | Risk | Validation Required | Decision |
|---|---|---|---|---|---|---|---|
| A1 | EFT JSON artifact will be generated by taurus (not JMeter itself or external tool) | ASSUME | Ticket says "Taurus-side: generate a json file"; no JMeter-side generation mentioned | Unknown whether existing JMeter-side EFT output exists; unclear if external tool is involved | Medium | Clarify during planning: does JMeter currently generate EFT JSON? If yes, reuse or replace? | **PROCEED** — ticket clearly states Taurus ownership; external tools are out of scope |
| A2 | Error categorization (ERRTYPE_ASSERT vs ERRTYPE_ERROR) can be inferred from Selenium/Apiritif framework error details | ASSUME | Apiritif code generator produces self.assertEqual, self.assertTrue calls (lines 509-527); JMeter find_failure logic detects assertions (lines 1425-1432) | Selenium/Apiritif frameworks may not consistently expose assertion names in error traces; may only have stack traces | High | **VALIDATION REQUIRED** — test Selenium/Apiritif error output to confirm assertion name extraction feasibility | **BLOCKED** — assertion name extraction from Selenium/Apiritif must be prototyped before finalizing spec |
| A3 | FunctionalSample + ResultsTree data structure is sufficient to capture per-transaction error details for EFT | ASSUME | FunctionalSample has status, error_msg, error_trace, subsamples fields (lines 67-92); ResultsTree aggregates by test suite | FunctionalSample.error_msg is a single string; may not have structured assertion metadata; subsamples field may not be populated for assertion failures | Medium | **VALIDATION REQUIRED** — verify FunctionalSample.subsamples is used by Selenium/Apiritif; if not, explore error_trace parsing | **NEEDS_APPROVAL** — planner must confirm FunctionalSample structure is rich enough |
| A4 | a.blazemeter.com Master::isTransactionFilterAllowed() gate can be updated to allow SCRIPT_TYPE_SELENIUM without side effects | ASSUME | Gate is isolated method; adding SELENIUM to allowed list is a simple array_diff change (line 3639) | Unknown downstream dependencies; other code may assume SELENIUM is excluded from EFT | Medium | **VALIDATE** — grep for "isTransactionFilterAllowed" usage and review all callers | **NEEDS_APPROVAL** — backend team must confirm gate update is safe |
| A5 | EFT JSON artifact is uploaded as a file within artifacts.zip (not a separate HTTP request) | ASSUME | All artifacts go through __upload_artifacts() which zips them (line 248) | External-results-import endpoint may expect separate upload; unknown final format/location | Medium | **VERIFY** — check external-results-import route handler and a.blazemeter.com upload expectations | **NEEDS_APPROVAL** — Sparta team must specify upload mechanism |
| A6 | Response bodies (for error debugging) will be available in Selenium/Apiritif result data with same structure as JMeter | ASSUME | JMeter KPISet includes responseBodies list (line 278) | Selenium/Apiritif frameworks may not capture/expose response bodies; browser-based testing may not have meaningful "response body" concept | High | **VALIDATION REQUIRED** — check if Selenium/Apiritif error results include response body data | **BLOCKED** — responseBodies for browser tests must be clarified before implementation |
| A7 | Taurus has no existing EFT-related config; feature will be added as new config section | ASSUME | grep -r "exclude\|eft" in blazemeter_reporter.py yields no EFT-specific settings | Config schema may already exist in base-config.yml or elsewhere; unknown if Sparta has upstream guidance | Low | **VERIFY** — check bzt/resources/10-base-config.yml for any "failed-transactions" or "eft" settings | **PROCEED** — planner can design config schema as needed; risk is low |
| A8 | Assertion failure names can be extracted reliably from Selenium/Apiritif test code (via AST or string parsing) | ASSUME | Apiritif generator creates assertion calls like "self.assertEqual(x, y)" with variable naming (lines 509-551); names may be inferred from assertion parameters | Browser-based assertions may not have explicit names; assertion failure output may only be "AssertionError: expected X got Y" | High | **VALIDATION REQUIRED** — prototype assertion name extraction from Apiritif/Selenium error output | **BLOCKED** — assertion naming strategy must be proven before spec finalizes |

---

## Boundary Compatibility Analysis

### Producer (taurus) → Consumer (a.blazemeter.com) Boundary

| Boundary | Producer Shape | Consumer Expectation | Adapter Required? | Evidence |
|---|---|---|---|---|
| JSON artifact file naming | taurus generates "failed_transactions.json" (initial) | a.blazemeter.com external-results-import expects consistent filename or reads from upload manifest | **Unknown** | No evidence of filename convention yet; planner coordinates with Sparta (A-SPARTA dependency task) |
| Error type enum (0=ERROR, 1=ASSERT, 2=SUBSAMPLE) | KPISet ERRTYPE_* constants (int enums) | a.blazemeter.com interprets error.type as int matching same values | **No adapter needed** — backend can reuse same enum interpretation | `/bzt/modules/aggregator.py:225-227`, `/bzt/modules/blazemeter/blazemeter_reporter.py:699-717` (uploader already uses same enum) |
| Timestamp precision (seconds vs milliseconds) | FunctionalSample.start_time (Python float, epoch seconds) | a.blazemeter.com timestamp field (likely epoch seconds or milliseconds) | **Verify** — planner must confirm expected precision | `/bzt/modules/functional.py:86` (start_time), `/bzt/modules/blazemeter/blazemeter_reporter.py:639` (reportInfo.timestamp: time.time()) |
| URL encoding in error data | Counter of URLs (plain strings, may contain special chars) | a.blazemeter.com JSON ingestion (expects URL-safe strings or JSON-escaped) | **Verify** — JSON encoder handles escaping; no custom adapter needed | `/bzt/modules/aggregator.py:277` (urls field), standard json.dumps() escaping |
| Assertion names (if present) | Synthetic name inferred from error trace or test code | a.blazemeter.com assertion filtering logic (unknown if it expects specific naming convention) | **Unknown** — planner must coordinate naming scheme with backend | `/bzt/modules/jmeter.py:1425` (assertion name extraction from JMeter tag), `/bzt/modules/_apiritif/generator.py:509-551` (assertion code generation) |

---

## Design Alternatives Considered

| Option | What Changes | Pros | Cons/Risks | Evidence | Decision |
|---|---|---|---|---|---|
| **Option A: Leverage existing JTLErrorsReader logic for Selenium (RECOMMENDED)** | Refactor JTLErrorsReader into generic error extraction + classification layer; create SeleniumErrorsReader subclass/adapter | Reuses battle-tested error categorization logic (ERRTYPE_* enums, assertion detection); minimizes new code; consistent with existing architecture | JTLErrorsReader is tightly coupled to JTL XML format; refactoring may introduce regressions; Selenium/Apiritif results are LDJSON/memory-based, not XML files | `/bzt/modules/jmeter.py:1255-1454` (JTLErrorsReader), `/bzt/modules/functional.py:130-145` (FunctionalResultsReader interface) | **SELECTED** — refactoring to shared base class for error classification is low-risk; concrete readers remain executor-specific |
| **Option B: Generate EFT JSON in aggregator (ConsolidatingAggregator)** | Add EFT artifact generation to ConsolidatingAggregator.post_process(); read from cumulative KPISet errors at end of test | Centralized aggregation point; EFT JSON mirrors KPI payload structure; no new module/reporter needed | Mixes concerns (KPI aggregation + EFT-specific output); tight coupling to KPISet schema; may not capture transaction-level details needed for filtering | `/bzt/modules/aggregator.py:863-920` (ConsolidatingAggregator), `/bzt/modules/blazemeter/blazemeter_reporter.py:657-717` (DatapointSerializer already builds structured error output) | **REJECTED** — aggregator is load-test focused; functional results need separate handling |
| **Option C: Generate EFT JSON in BlazeMeterUploader (RECOMMENDED)** | Add EFT artifact generation to BlazeMeterUploader.post_process() or __upload_artifacts(); iterate over aggregator.cumulative or functional aggregator; write file to artifacts_dir | Follows existing upload pattern; natural home for BlazeMeter-specific output; can access both KPI and functional results | BlazeMeterUploader already has large responsibility (KPI sending + monitoring + artifact upload); adds more logic; may not have clean access to functional aggregator in all modes | `/bzt/modules/blazemeter/blazemeter_reporter.py:300-349` (post_process), `/bzt/modules/__init__.py:23` (engine.aggregator is ConsolidatingAggregator, which can hold both KPI and functional) | **SELECTED** — BlazeMeterUploader already coordinates upload; natural extension point |
| **Option D: Generate EFT JSON in a new reporter (SeleniumReporter / FunctionalReporter subclass)** | Create standalone Reporter that listens to FunctionalAggregator and generates EFT JSON independently | Clean separation of concerns; reusable for non-BlazeMeter scenarios; easier to test in isolation | Adds new module to codebase; potential duplication if JMeter EFT is already in a reporter elsewhere; may duplicate logic with DatapointSerializer | `/bzt/modules/__init__.py:35-36` (Reporter base class) | **MAYBE** — secondary option if BlazeMeterUploader approach becomes unwieldy; deferred to planner decision |
| **Option E: Let a.blazemeter.com generate EFT JSON from raw KPI/error data** | taurus uploads raw error data structures (JSON array of KPISet error items); a.blazemeter.com applies EFT filtering and format transformation | Simplifies taurus; backend maintains format flexibility; less coupling between repos | Violates ticket requirement ("Taurus-side: generate a json file"); high latency (filtering happens post-upload); less transparent to taurus users | Ticket requirement section clearly states "generate a json file" as taurus responsibility | **REJECTED** — contradicts ticket scope |

---

## Test Validity Strategy

**Required Test Shape:**
1. **Unit test:** Mock FunctionalSample/ResultsTree with various error scenarios (assertion, HTTP error, subsample); verify EFT JSON generation produces correct error type, message, assertion name
2. **Integration test:** Run actual Selenium/Apiritif test with intentional failures (assertion + HTTP error); verify EFT JSON artifact is created and contains expected errors
3. **Upload test:** Verify EFT JSON file is included in artifacts.zip and uploaded to BlazeMeter session
4. **Schema validation test:** Verify generated EFT JSON conforms to Sparta-defined schema (once available)

**Mock-Reality Rule (what MUST NOT be mocked):**
- **DO NOT mock:** FunctionalSample structure or ResultsTree aggregation — use real framework output (LDJSON or memory objects from actual apiritif/pytest run)
- **DO NOT mock:** BlazeMeter uploader session — use real (or test stub) BlazeMeter API
- **CAN mock:** Sparta JSON schema validation (until schema is finalized); database/persistence (if EFT metadata is stored backend-side)

**Assumption-Challenge Case:**
- Test Selenium + Apiritif assertion failures with framework that provides BOTH assertion name AND error trace; verify assertion name extraction logic correctly parses name from error output
- Test Selenium + HTTP error (e.g., 404 in sub-request); verify rc (response code) field is correctly set to "404" and error type is ERRTYPE_ERROR (not ERRTYPE_ASSERT)
- Test Selenium + assertion failure WITH response body available; verify responseBodies array is populated (or confirm it's null for browser tests)

**Evidence (Existing Tests to Extend):**
- `/bzt/tests/unit/modules/test_blazeMeterUploader.py` — add EFT JSON generation and upload tests
- `/bzt/tests/unit/modules/test_FunctionalAggregator.py` (or similar) — add EFT JSON schema validation tests
- `/bzt/tests/resources/selenium/` — add Selenium test scripts with assertion failures for integration testing
- `/bzt/tests/unit/modules/jmeter/test_JTLReader.py:375-463` — examples of error type assertions; adapt for Selenium

---

## Current State

### Relevant Files (file:line-range — one-line desc)

- `/bzt/modules/aggregator.py:206-279` — KPISet class and error_item_skel definition (error structure)
- `/bzt/modules/jmeter.py:1255-1454` — JTLErrorsReader class (error extraction + ERRTYPE classification)
- `/bzt/modules/functional.py:67-145` — FunctionalSample, ResultsTree, FunctionalAggregator (functional test result tracking)
- `/bzt/modules/_apiritif/executor.py:52-248` — ApiritifNoseExecutor (transaction tracking via stdout parsing)
- `/bzt/modules/__init__.py:102-126` — ReportableExecutor with transaction subscription interface
- `/bzt/modules/_selenium.py:33-200` — SeleniumExecutor (delegates to runner + subscribes to transactions)
- `/bzt/modules/blazemeter/blazemeter_reporter.py:55-298` — BlazeMeterUploader init, prepare, post_process, artifact upload
- `/home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Model/Master.php:3629-3644` — Master::isTransactionFilterAllowed() gate
- `/home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Helpers/TaurusConfiguration/TaurusExecution.php:40-57` — executor type constants

### Root Cause / Gap

**Current EFT Feature State:**
- JMeter: EFT works (error extraction from TJL, categorization, reporting to a.blazemeter.com)
- Selenium: NOT YET IMPLEMENTED
  - SeleniumExecutor has no EFT artifact generation
  - BlazeMeterUploader has no EFT JSON generation logic
  - a.blazemeter.com Master::isTransactionFilterAllowed() explicitly forbids SCRIPT_TYPE_SELENIUM (line 3639)

**Why the Gap Exists:**
- Selenium/Apiritif results flow through functional aggregator (FunctionalAggregator, FunctionalSample), not load-test aggregator (ConsolidatingAggregator + KPISet)
- Functional aggregator does not have EFT artifact generation
- No shared error classification layer between JMeter and Selenium paths

---

## Desired State Delta

| What | File | Change |
|---|---|---|
| Error classification logic (ERRTYPE_* categorization) | `/bzt/modules/jmeter.py` and new shared module | Refactor JTLErrorsReader error classification (find_failure, get_child_assertion, parse_assertion) into reusable utility functions or base class |
| SeleniumErrorsReader or SeleniumErrorAggregator | new file `/bzt/modules/functional_eft.py` or extend functional.py | Implement error categorization for FunctionalSample (map FAILED/BROKEN status + error_msg/error_trace to ERRTYPE_ERROR/ERRTYPE_ASSERT; extract assertion names) |
| EFT artifact generation | `/bzt/modules/blazemeter/blazemeter_reporter.py` | Add method to generate failed_transactions.json from aggregator results (KPISet for JMeter + FunctionalSample for Selenium); call from post_process |
| EFT artifact output | engine.artifacts_dir | Create "failed_transactions.json" file (or Sparta-specified name) during post_process; include in artifacts.zip upload |
| BlazeMeter backend gate update | `/home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Model/Master.php:3639` | Add SCRIPT_TYPE_SELENIUM to allowed executor list in array_diff check |
| Config schema | `/bzt/resources/10-base-config.yml` | Add blazemeter.generate-failed-transactions config option (bool, default: true if EFT feature flag is enabled on account) |
| Tests | `/bzt/tests/unit/modules/test_blazeMeterUploader.py` and new files | Add unit tests for EFT JSON generation; integration tests for Selenium + assertion failures |

---

## Files the Planner Must Read for spec.md

1. `/bzt/modules/aggregator.py:206-379` — KPISet, error_item_skel, error structure deep dive
2. `/bzt/modules/jmeter.py:1255-1454` — JTLErrorsReader, error classification logic (model to reuse)
3. `/bzt/modules/functional.py:67-145` — FunctionalSample, FunctionalAggregator (parallel aggregation path for Selenium)
4. `/bzt/modules/blazemeter/blazemeter_reporter.py:234-298, 300-349` — artifact generation and upload flow
5. `/home/jenkins/blazemeter/a.blazemeter.com/src/blazemeter/Model/Master.php:3629-3644` — backend gate logic (required update)
6. `/bzt/modules/_apiritif/executor.py:231-248` — transaction lifecycle integration (how apiritif reports transaction start/end)
7. `/bzt/modules/__init__.py:102-126` — ReportableExecutor transaction subscription interface
8. `/bzt/modules/_selenium.py:33-70` — SeleniumExecutor delegation to runner
9. `/bzt/modules/_apiritif/generator.py:504-556, 2293-2303` — assertion code generation (for assertion name extraction strategy)

---

## Planner Blockers

- **none** — all four items originally flagged have been resolved to non-blocking status before /specify. They survive as explicit VALIDATE assumptions and dependency/contract-verification tasks (see resolved_ambiguities and Assumption Ledger), NOT as planning blockers:

- **RESOLVED (was BLOCKER 1):** Assertion name extraction from Selenium/Apiritif — resolved confidence-first as a VALIDATE assumption. plan.md/tasks.md carry a contract-verification/POC task that reads a real assertion-failure fixture and proves the assertion name is recoverable; degrade to a synthetic name derived from the transaction label if not recoverable. Reuse the existing ERRTYPE_ASSERT classification.

- **RESOLVED (was BLOCKER 2):** Sparta schema approval — the reviewer handoff already documents Sparta ownership of the final format as an ACCEPTED, NON-BLOCKING dependency ("Titans can implement Selenium-side work using an initial format now, final format/code requires review and approval from Sparta before finalization"). The plan uses an initial format mirroring the proven error_item_skel / DatapointSerializer split and carries a "Sparta schema sign-off" dependency/validation task.

- **RESOLVED (was BLOCKER 3):** Response body capture for browser tests — resolved confidence-first. Selenium/Apiritif browser tests do not produce JMeter-style response bodies; the initial format emits an empty responseBodies list (per Negative Constraint on graceful degradation). Documented as an assumption, not a blocker.

- **RESOLVED (was BLOCKER 4):** a.blazemeter.com ingestion handler — this is the a.blazemeter.com repo's concern and is traced in that repo's own brownfield research. For taurus (the producer), the contract is "write failed_transactions.json to artifacts_dir following the proven error_item_skel shape". Cross-repo verification is tracked as a dependency in the a.blazemeter.com plan, not a taurus blocker.

---

## overall_finding

**brownfield** — EFT feature for JMeter already exists (in a.blazemeter.com backend gating and taurus error classification logic); Selenium support is new but reuses existing error categorization infrastructure (KPISet ERRTYPE_* enums, assertion detection patterns from JMeter). No dangerous naming conflicts identified. Key risk is functional test assertion name extraction (unvalidated) and Sparta schema approval (external dependency).

### Summary

**Overall Finding:** brownfield (existing EFT for JMeter; Selenium is new capability)

**Forbidden/Ambiguous Identifier Count:**
- Forbidden: 0
- Ambiguous: 3 (EFT artifact file location/naming, whether EFT is opt-in or always-on, assertion name extraction strategy)

**Existing jmx EFT JSON Analog Found:** Partial
- Error structure and categorization logic found: `/bzt/modules/aggregator.py:266-279` (error_item_skel), `/bzt/modules/jmeter.py:1413-1454` (error classification)
- Uploader path found: `/bzt/modules/blazemeter/blazemeter_reporter.py:268-298` (artifacts.zip generation)
- Actual EFT JSON artifact generation in taurus: NOT FOUND (likely generated downstream by JMeter or a.blazemeter.com, not in taurus codebase)

**Planner Blockers:**
1. Assertion name extraction from Selenium/Apiritif unvalidated
2. Sparta schema approval required before implementation
3. Response body availability in browser tests unclear
4. External-results-import handler in a.blazemeter.com not yet located

