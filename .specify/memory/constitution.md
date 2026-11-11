<!--
Sync Impact Report
==================
Version change: (template) → 1.0.0
Rationale: Initial ratification. All placeholders in the template have been
replaced with concrete Taurus-specific principles and governance.

Modified principles:
- [PRINCIPLE_1_NAME] → I. YAML-First Unified Interface
- [PRINCIPLE_2_NAME] → II. Wrapper Fidelity Over Reinvention
- [PRINCIPLE_3_NAME] → III. Test-First (NON-NEGOTIABLE)
- [PRINCIPLE_4_NAME] → IV. Module Lifecycle Discipline
- [PRINCIPLE_5_NAME] → V. Observable, Aggregatable Results

Added sections:
- Additional Constraints (replaces [SECTION_2_NAME])
- Development Workflow & Quality Gates (replaces [SECTION_3_NAME])
- Governance (populated)

Removed sections: none

Templates requiring updates:
- ✅ .specify/templates/plan-template.md (Constitution Check gate is
  principle-agnostic; no textual change required)
- ✅ .specify/templates/spec-template.md (no constitution-specific text)
- ✅ .specify/templates/tasks-template.md (no constitution-specific text)
- ✅ .specify/templates/checklist-template.md (no constitution-specific text)

Follow-up TODOs: none
-->

# Taurus (bzt) Constitution

## Core Principles

### I. YAML-First Unified Interface

Taurus exposes every capability through the unified YAML configuration surface
consumed by the `bzt` CLI. New executors, services, reporters, and provisioners
MUST register in `bzt/resources/10-base-config.yml` and be reachable by a stable
YAML name. Tool-specific quirks (JMeter GUI XML, Gatling Scala DSL, Playwright
scripts, etc.) MUST remain hidden from the user's YAML unless they are
explicitly opted into via a `raw`/`script` escape hatch. Rationale: Taurus's
value proposition is a single YAML dialect across many test tools; leaking
tool-native config into the user surface erodes portability.

### II. Wrapper Fidelity Over Reinvention

Taurus wraps existing tools (JMeter, Gatling, Locust, Selenium, Apiritif,
Playwright, etc.); it MUST NOT reimplement their execution engines. Executor
modules SHOULD invoke the upstream tool via its supported entry point and parse
its native output files (JTL, CSV, JSON) rather than duplicate its logic. When
upstream behavior is buggy or missing, prefer a documented shim with a link to
the upstream issue over a fork. Rationale: reimplementation multiplies the
maintenance surface and diverges from upstream fixes.

### III. Test-First (NON-NEGOTIABLE)

Every change to `bzt/` MUST land with unit tests under `tests/unit/` written
against `BZTestCase` and `EngineEmul`. New executors MUST include a fixture
under `tests/resources/` and a smoke test that runs `prepare()` at minimum.
Bug fixes MUST include a regression test that fails before the fix and passes
after. The `nose2` suite (`python -m nose2 -s tests/unit -v`) MUST pass on the
target branch before merge; coverage regressions require explicit
justification in the PR. Rationale: Taurus orchestrates many external tools —
without tests, silent breakage in one executor is invisible until a customer
run fails.

### IV. Module Lifecycle Discipline

All modules MUST extend the appropriate `EngineModule` subclass
(`ScenarioExecutor`, `Provisioning`, `Aggregator`, `Reporter`, `Service`) and
confine work to the documented lifecycle:
`prepare()` → `startup()` → polling → `shutdown()` → `post_process()`.
Blocking work, subprocess launches, and network I/O MUST NOT run at import
time or in `__init__`. Resources acquired in `prepare()`/`startup()` MUST be
released in `shutdown()`/`post_process()` even on error paths. Rationale: the
`Engine` coordinates many modules concurrently; lifecycle violations cause
hangs, orphaned processes, and partial cleanups that are hard to diagnose.

### V. Observable, Aggregatable Results

Any module that produces test results MUST emit them as `DataPoint` objects
via a `ResultsReader` that the `ConsolidatingAggregator` can consume. Custom
result formats are permitted on disk, but the in-memory contract to
aggregators and reporters MUST use the standard `DataPoint` schema. Reporters
MUST NOT reach into executor internals; they consume aggregated `DataPoint`
streams only. Errors and warnings MUST be logged through the module's logger,
never silently swallowed. Rationale: reporters (console, JUnit, BlazeMeter
uploader, InfluxDB) rely on a single aggregated stream; bypassing it produces
inconsistent dashboards and broken pass/fail gates.

## Additional Constraints

**Runtime**: Python 3 as declared in `setup.py`; new code MUST run on the
Python versions covered in `tests/ci/`.
**Dependencies**: Runtime dependencies MUST be declared in
`requirements.txt`; test-only dependencies in `tests/ci/requirements.txt`.
Adding a new runtime dependency requires justification in the PR.
**CLI stability**: The public CLIs — `bzt`, `jmx2yaml`, `soapui2yaml`,
`swagger2yaml` — have stable flag surfaces. Removing or renaming a flag
requires a MAJOR version bump and a deprecation notice for at least one
minor release.
**Cross-platform**: Core `bzt` execution MUST work on Linux, macOS, and
Windows. Platform-specific code MUST be guarded and covered by tests on that
platform or explicitly marked as unsupported.
**Security**: User-supplied YAML MUST NOT be `eval`'d or `exec`'d. Shell
commands built from user input MUST use argument lists, not shell string
concatenation.

## Development Workflow & Quality Gates

**Local dev setup** follows the project `CLAUDE.md`:
`pip install -r requirements.txt -r tests/ci/requirements.txt && pip install -e .`.
**Test gate**: `python -m nose2 -s tests/unit -v` MUST pass before merge.
**Coverage gate**: CI runs `coverage run --source=bzt -m nose2 -s tests/unit`;
significant drops (>1%) require justification.
**Build gate**: `./build-artifacts.sh` MUST succeed on the release branch
before tagging.
**Review**: Every PR requires at least one maintainer review. PRs touching
the module registry (`10-base-config.yml`), CLI entry points, or the
`DataPoint`/aggregator contract require two reviews.
**Docs**: Changes to user-visible YAML keys, CLI flags, or default behavior
MUST update the corresponding page under `site/dat/docs/` in the same PR.

## Governance

This constitution supersedes ad-hoc conventions and undocumented practice.
Amendments MUST be proposed via PR that (a) updates
`.specify/memory/constitution.md`, (b) bumps the version per the policy
below, (c) updates the Sync Impact Report at the top of this file, and (d)
identifies any dependent templates or docs that need to change in the same
PR.

**Versioning policy** (semantic):
- **MAJOR**: Removing or redefining a principle; breaking governance change;
  removing a public CLI flag or YAML key.
- **MINOR**: Adding a new principle or materially expanding an existing one;
  adding a new mandatory section.
- **PATCH**: Clarifications, wording, typo fixes, non-semantic refinements.

**Compliance review**: All PRs MUST verify compliance with the principles
above. Deviations MUST be recorded in the `Complexity Tracking` section of
the feature plan with the simpler alternative that was rejected and why.
Repeated deviations in the same subsystem trigger a constitutional review
at the next release.

**Runtime guidance**: Day-to-day development guidance for AI assistants and
new contributors lives in `CLAUDE.md` at the repository root; that file MUST
remain consistent with this constitution.

**Version**: 1.0.0 | **Ratified**: 2026-08-18 | **Last Amended**: 2026-08-18
