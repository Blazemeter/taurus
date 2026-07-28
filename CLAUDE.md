# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Is

Taurus (`bzt`) is a performance and functional test automation framework. It wraps underlying tools (JMeter, Gatling, Locust, Selenium, Apiritif, Playwright, etc.) behind a unified YAML-based configuration interface. The `bzt` CLI runs test plans; separate entry points (`jmx2yaml`, `soapui2yaml`, `swagger2yaml`) convert external formats to Taurus YAML.

## Development Setup

```bash
pip install -r requirements.txt -r tests/ci/requirements.txt
pip install -e .
```

## Commands

**Run all unit tests:**
```bash
python -m nose2 -s tests/unit -v
```

**Run a single test file:**
```bash
python -m nose2 -s tests/unit tests.unit.modules.test_Gatling -v
```

**Run a single test method:**
```bash
python -m nose2 -s tests/unit tests.unit.modules.test_Gatling.TestGatlingExecutor.test_install_Gatling -v
```

**Run with coverage (as CI does):**
```bash
coverage run --source=bzt -m nose2 -s tests/unit -v
coverage report -m
```

**Build a wheel:**
```bash
./build-artifacts.sh
```

## Architecture

### Core Engine (`bzt/engine/`)

The `Engine` class (`engine.py`) is the central coordinator. It owns:
- `config` (`Configuration`) — merged YAML config tree
- `provisioning` — how to run executors (local or cloud)
- `aggregator` — collects and merges KPI data from all executors
- `reporters` — list of `Reporter` instances (final status, JUnit XML, console, etc.)
- `services` — ancillary `Service` instances (monitoring, shell exec, install checker, etc.)
- `modules` — dict of registered module classes (populated from `10-base-config.yml`)

**Module lifecycle** (called by `Engine` on all active modules):
`prepare()` → `startup()` → (polling loop) → `shutdown()` → `post_process()`

### Module Base Classes (`bzt/engine/modules.py`)

All modules extend `EngineModule`. Specializations:
- `ScenarioExecutor` — runs a load/functional test (JMeter, Gatling, Locust, Selenium, Playwright, etc.)
- `Provisioning` — controls how executors are launched (`Local` or BlazeMeter Cloud)
- `Aggregator` — receives and merges `DataPoint` samples
- `Reporter` — produces output (console, final status, JUnit XML, InfluxDB, BlazeMeter uploader)
- `Service` — background helper (monitoring, shell hooks, proxy2jmx, etc.)

`ReportableExecutor` (`bzt/modules/__init__.py`) bridges executors that also report functional results.

### Module Registration

`bzt/resources/10-base-config.yml` is the canonical module registry — it maps YAML executor names (`jmeter`, `gatling`, `selenium`, `playwright`, etc.) to Python classes. This file is always loaded first; user YAML merges on top.

### JMeter Support (`bzt/jmx/`)

The `bzt/jmx/` package handles JMeter XML generation. It has separate protocol handlers (`http.py`, `mqtt.py`, `grpc.py`) and thread-group builders (`threadgroups.py`). The `JMeterExecutor` in `bzt/modules/jmeter.py` orchestrates these.

### BlazeMeter Cloud (`bzt/modules/blazemeter/`)

Cloud provisioning, result uploading, and project/test management are isolated in this sub-package. `bza.py` at the top level is the low-level REST API client.

### Data Flow

Executors write results to files (JTL, CSV, etc.). Each executor attaches a `ResultsReader` that tails that file and emits `DataPoint` objects. The `ConsolidatingAggregator` merges these across all executors and passes aggregated `DataPoint`s to every `AggregatorListener` (reporters, pass/fail checker, etc.).

### Test Infrastructure (`tests/unit/`)

- `tests/unit/base.py` — constants (`BZT_DIR`, `RESOURCES_DIR`, `BUILD_DIR`, `TEST_DIR`) and path helpers
- `tests/unit/cases.py` — `BZTestCase(TestCase)` base class with log capture helpers
- `tests/unit/mocks.py` — `EngineEmul` (in-memory engine for tests), mock readers, fake data generators

Tests inherit from `BZTestCase`, construct an `EngineEmul`, load YAML fixtures from `tests/resources/`, and assert on engine state or output files.
