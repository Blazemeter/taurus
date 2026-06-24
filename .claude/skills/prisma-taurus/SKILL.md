---
name: prisma-taurus
description: Use when the user wants to check, fix, or verify Prisma Cloud vulnerability scan results for the Taurus Docker image (blazemeter/taurus:unstable). Run this skill on Opus 4.8 (1M context) — choosing and applying the correct CVE fix requires strong reasoning.
---

## Model requirement — run on Opus 4.8 (1M context)

> **Before doing anything else, confirm the active model is Opus 4.8 (1M context) (`claude-opus-4-8[1m]`). If it is not, STOP and tell the user to switch with `/model` (choose Opus 4.8) and re-invoke `/prisma-taurus` — do not proceed on a smaller model.**
>
> Why: this skill makes real, hard-to-verify decisions — classifying each CVE by `Path`, choosing the right fix mechanism, recognizing traps like the Ruby default-gem problem or scanner appeasement, and reasoning about whether a fix will actually land in the image before opening a PR. Weaker models miss these and produce fixes that silently don't work. Skills run on the session's active model (there is no frontmatter field that forces a model), so the model must be set by the user before running.

## Overview

End-to-end vulnerability management for the Taurus Docker image. Fetches the latest Prisma Cloud scan, classifies findings into auto-fixable and manual categories, applies all safe fixes, runs unit tests, then **builds the fixed branch into a Docker image and re-scans it** to confirm the fixes actually reduce vulnerabilities in the real image. A PR is opened **only after** that branch scan shows fewer vulnerabilities than the baseline — never at an early stage.

**Why verify before the PR:** unit tests do not exercise the Dockerfile (gem/npm/apt changes) and a fix that "looks" applied can be a no-op against the scanner (e.g. `gem update` installs a patched gem but leaves the vulnerable Ruby *default-gem* version on disk, which Prisma still reports). The `taurus-branch-builder` Jenkins job can build any branch into an image and scan it with Prisma — so the fixes are proven in the image *before* a PR is created, not assumed.

## When to Use

- User asks about Taurus CVEs, vulnerabilities, or security scan results
- User wants to fix vulnerabilities found in `blazemeter/taurus`
- User wants to verify that a previous fix resolved a CVE
- User asks to run or check the Prisma scan for Taurus

## Common Mistakes

- Triggering a new scan when a valid one already exists (within 5 days and after last `unstable` push) — always check first
- **Fixing JMeter/Gatling-bundled CVEs** — any finding whose `Path` is under `/root/.bzt/jmeter-taurus/` or `/root/.bzt/gatling-taurus/` is out of scope. Do not repin jars and **do not bump `JMeter.VERSION` / `Gatling.VERSION`** (a version bump is a JMeter/Gatling fix). List them only.
- Auto-fixing scanner appeasement CVEs (`.deps.json`, `/var/lib/dpkg/` paths) — these must always be flagged as manual
- Removing the worktree after a test failure — leave it in place for the user to investigate
- **Opening the PR before the branch scan confirms a reduction** — push the branch, build+re-scan it via `taurus-branch-builder`, compare against the baseline, and create the PR ONLY if vulnerabilities went down. The PR is the last step, never an early one.
- **Trusting that a Dockerfile fix landed just because the build succeeded** — always confirm in the branch scan that each fixed package's vulnerable version is actually gone. `gem update` on a Ruby *default gem* (e.g. `net-imap`, `erb`) installs the patched version but leaves the old default-gem version on disk, which the scanner still reports.

## Image target

| Image | Registry | Tag | Artifact |
|---|---|---|---|
| `blazemeter/taurus` | Docker Hub (public) | `unstable` | `taurus.csv` (CSV) |

`unstable` is always the latest master build — pushed by Jenkins on every merge to master. This is the image to scan before a release.

## Credentials

Read from environment variables — never prompt, never hardcode:

```bash
JENKINS_USERNAME   # BlazeMeter Jenkins username
JENKINS_TOKEN      # Jenkins API token
GITHUB_TOKEN       # GitHub personal access token
```

If any variable is missing, stop and tell the user which one is absent.

**Jira** (used by the ticket-creation step before the PR) is accessed through the **Atlassian MCP**, not an env var — it authenticates as the developer running the skill (that's how the ticket gets assigned to them). No token to set. Note: interactively-authenticated MCP servers may be **absent in headless/cron runs**; if the Atlassian tools aren't available, create the PR without a Jira ticket and flag that the ticket must be created manually — don't block the PR.

## Fix classification rules

Before applying any fix, classify each CVE **by its `Path`** (the path is the definitive identifier of what the vulnerable component belongs to). Apply the categories in this order — the first match wins:

**1. JMeter / Gatling bundled → DO NOT FIX, monitor only:**
- `Path` contains `/root/.bzt/jmeter-taurus/` (e.g. `/root/.bzt/jmeter-taurus/5.5/lib/tika-core-1.28.3.jar`)
- `Path` contains `/root/.bzt/gatling-taurus/` (e.g. `/root/.bzt/gatling-taurus/3.9.5/lib/netty-codec-http-4.1.92.Final.jar`)

These jars (netty, log4j, tika, batik, xstream, jackson, logback, pebble, dnsjava, json-smart, json-path, commons-*, etc.) ship **inside** the JMeter/Gatling distributions. Taurus does not pin them individually, and **we do not remediate them** — not by replacing jars, and **not** by bumping the JMeter or Gatling version (a version bump is itself a JMeter/Gatling fix, only partially clears the CVEs, and carries runtime/behavioral risk). Display them in the report under "JMeter/Gatling — not fixed (out of scope)", never patch them.

**2. Scanner appeasement → flag as manual, never auto-fix:**
- `Path` contains `/usr/share/dotnet/sdk/` (Roslyn / .deps.json metadata patches)
- `Path` contains `/var/lib/dpkg/` (OS package version string manipulation)

**3. Auto-fixable → apply automatically (remaining CVEs where `Fix Status` starts with `fixed in`):**

| Path pattern | Fix type | Where to fix |
|---|---|---|
| `/root/.bzt/newman/node_modules/` | npm override | Dockerfile newman `package.json` printf block |
| `/root/.bzt/selenium-taurus/mocha/node_modules/` | npm override | Dockerfile mocha `package.json` printf block |
| `/root/.bzt/selenium-taurus/*/node_modules/` | npm direct package | `bzt/modules/javascript.py` PACKAGE_NAME constant |
| `/usr/local/rbenv/` or `/usr/local/lib/ruby/` | Ruby gem | Dockerfile `gem install <gem> -v <fixed>` + default-gem/cache cleanup (see Ruby gems section) |
| empty path or OS path (`/usr/lib/`, `/lib/`) | OS package | Dockerfile `apt-get install --only-upgrade` |
| Python pkg, `Path` under `/usr/local/lib/python3.x/...` | Python package | `requirements.txt` or `services.py` pip constraints — **classify into 5 cases**, see Python section |
| Python pkg, `Path` under `/usr/lib/python3/dist-packages/...` | distro Python (apt) | Dockerfile `apt-get --only-upgrade`/`purge` (NOT `requirements.txt`) |

> **Note:** the `/root/.bzt/jmeter-taurus/*` and `/root/.bzt/gatling-taurus/*` paths are intentionally **absent** from this auto-fixable table — they belong to category 1 (do not fix). Do not reintroduce JMeter/Gatling jar fixes or version bumps.

CVEs where `Fix Status` is not `fixed in ...` (e.g. `needed`, `deferred`, or empty) → display only, do not attempt to fix.

## Steps

### 1. Verify credentials

```bash
: "${JENKINS_USERNAME:?JENKINS_USERNAME is not set}" && \
: "${JENKINS_TOKEN:?JENKINS_TOKEN is not set}" && \
: "${GITHUB_TOKEN:?GITHUB_TOKEN is not set}" && \
echo "credentials OK"
```

### 2. Check if last scan is still valid

> **curl gotcha (`-g`):** Jenkins `tree=` queries contain `[` and `]`, which curl treats as a glob range — an un-escaped one fails with `bad range in URL`. The commands below URL-encode the brackets (`%5B`/`%5D`) and so are safe as written. If you ever write the brackets literally (as the reference table does for readability), add `-g` (globoff): `curl -sS -g -u "$JENKINS_USERNAME:$JENKINS_TOKEN" ".../api/json?tree=foo[bar]"`.

**Get the timestamp when `unstable` was last pushed to Docker Hub:**
```bash
curl -s "https://hub.docker.com/v2/repositories/blazemeter/taurus/tags/unstable" \
  | python3 -c "import json,sys; d=json.load(sys.stdin); print(d['last_updated'])"
```

**Find the most recent successful Jenkins scan for taurus:unstable:**
```bash
curl -sL -u "$JENKINS_USERNAME:$JENKINS_TOKEN" \
  "https://blazect-jenkins.blazemeter.com/job/prisma-cloud-ondemand-scan/api/json?tree=builds%5Bnumber%2Ctimestamp%2Cresult%2Cactions%5Bparameters%5Bname%2Cvalue%5D%5D%5D"
```

Look for the most recent build where:
- `result` is `SUCCESS`
- The `IMAGE_URL` parameter value is `blazemeter/taurus:unstable`

**The scan is valid only if BOTH are true:**
- Scan timestamp is **after** the `unstable` image push timestamp (correctness — the scan must reflect the current master image)
- Scan ran **today** (same UTC calendar day as now) — freshness, since CVE feeds update daily and a re-scan of an unchanged image surfaces newly-disclosed CVEs

If valid → skip to step 4 using that build number.

Otherwise (no scan today, even if the image is unchanged) → continue to step 3 and trigger a fresh scan.

**First check if a scan is already running:**
```bash
curl -sL -u "$JENKINS_USERNAME:$JENKINS_TOKEN" \
  "https://blazect-jenkins.blazemeter.com/job/prisma-cloud-ondemand-scan/api/json?tree=builds%5Bnumber%2Ctimestamp%2Cresult%2Cbuilding%2Cactions%5Bparameters%5Bname%2Cvalue%5D%5D%5D"
```
If the most recent build has `building: true` and `IMAGE_URL` is `blazemeter/taurus:unstable` → tell the user a scan is already running, wait for it (poll every 20 seconds), then use that build's result. If it finishes with `FAILURE` → stop and give the user the console URL.

### 3. Trigger a new scan (one time only — no retries)

```bash
curl -s -o /dev/null -D - -X POST \
  -u "$JENKINS_USERNAME:$JENKINS_TOKEN" \
  "https://blazect-jenkins.blazemeter.com/job/prisma-cloud-ondemand-scan/buildWithParameters?IMAGE_URL=blazemeter/taurus:unstable"
```

Capture the `Location:` header — it contains the queue item URL (e.g. `.../queue/item/XXXXX/`).

**Wait for build number** by polling the queue item every 10 seconds until `executable.number` appears:
```bash
curl -sL -u "$JENKINS_USERNAME:$JENKINS_TOKEN" \
  "https://blazect-jenkins.blazemeter.com/queue/item/<ID>/api/json"
```

**Wait for the build to finish** by polling every 20 seconds. Tell the user the build number and that you are waiting (typically 10–20 minutes):
```bash
curl -sL -u "$JENKINS_USERNAME:$JENKINS_TOKEN" \
  "https://blazect-jenkins.blazemeter.com/job/prisma-cloud-ondemand-scan/<BUILD>/api/json?tree=building,result"
```

If `result` is `FAILURE` → stop and give the console URL:
`https://blazect-jenkins.blazemeter.com/job/prisma-cloud-ondemand-scan/<BUILD>/console`

### 4. Fetch the scan artifact

```bash
curl -sL -u "$JENKINS_USERNAME:$JENKINS_TOKEN" \
  "https://blazect-jenkins.blazemeter.com/job/prisma-cloud-ondemand-scan/<BUILD>/artifact/taurus.csv"
```

### 5. Parse, classify, and display

Order by severity: **critical → high → medium → low/unassigned**, then by CVSS descending within each severity.

Display a table with columns:
- CVE ID
- Severity
- CVSS
- Package + Version
- Fix Status
- Fix Type (`auto-fix` / `jmeter-gatling (out of scope)` / `manual` / `no fix available`)

Show a summary line:
> `X vulnerabilities — Y critical, Z high, N medium, M low — A auto-fixable, J jmeter/gatling (out of scope), B manual, C no fix available`

### 6. Collect auto-fixable CVEs

Gather all CVEs where:
- `Path` is NOT under `/root/.bzt/jmeter-taurus/` or `/root/.bzt/gatling-taurus/` (those are out of scope — category 1)
- `Fix Status` starts with `fixed in`
- Path does NOT match scanner appeasement patterns

If there are no auto-fixable CVEs → report that and list any manual items, then stop.

### 7. Apply all auto-fixes

Work in priority order: critical → high → medium → low. For each CVE apply the fix according to the classification table above.

---

#### Before adding new fixes — prune stale ones

Past runs left temporary fixes in the Dockerfile and `requirements.txt`: npm `overrides` blocks, forced `gem install` + default-gem purges, `apt-get --only-upgrade` pins, `.deps.json`/dpkg `sed` patches, and any `pip install` step outside the normal requirements install. Before adding anything new, re-evaluate each one:

- **Does the CVE it was added for still appear in the current baseline scan (step 5)?** If the parent package or base image now ships the fixed version (the CVE is gone from the baseline), **remove that line on the fix branch.** The branch re-scan (step 12) is the safety net — if a removal regresses, the scan catches it before the PR.
- If the CVE is still present → leave the fix in place.

Check `vulnerability_history.md` for any recorded removal condition before deciding (see step 13).

**Convention — mark every temporary fix with its removal condition.** Each temporary fix this skill adds must carry an inline `# remove when <upstream condition>` comment (e.g. `# remove when nodejs ships npm with glob >= 11.1.0`) so a future run can find and re-evaluate it. A self-pruning runtime guard — like the Newman `NEWMAN_VERSION = 6.2.2` check that skips the override once Newman moves — satisfies this too. When the condition is non-obvious, also record it under the relevant recipe in `vulnerability_history.md`.

---

#### Python packages — classify by `Path` (not every Python fix is `requirements.txt`)

Decide the mechanism from the finding's `Path`:

1. **Pinned in `requirements.txt`** — `Path` under `/usr/local/lib/python3.x/...` and the package name *is* in `requirements.txt` → bump the existing pin. Extract the fixed version from `Fix Status` (`fixed in 2.7.0` → `2.7.0`):
   ```
   urllib3==2.6.3   →   urllib3==2.7.0
   ```
2. **Transitive dep not in `requirements.txt`** — `Path` under `/usr/local/lib/python3.x/...` but the package name is absent from `requirements.txt` → force the fixed version with a new pin. Prefer adding it to `PipInstaller.pip_constraints` in `bzt/modules/services.py` (the constraints file is applied to every pip install); add a direct `requirements.txt` line only if it is genuinely a direct dependency.
3. **Resolver constraint blocks the pin** — another dependency caps the version, so re-resolving `requirements.txt` fails → add a dedicated `pip install <pkg>==<fixed>` step in the Dockerfile right after the BZT-install pip step, with a `# remove when <condition>` marker (see the pruning convention above).
4. **Vendored inside setuptools** — `Path` contains `/setuptools/_vendor/...` → cannot be pinned directly; only bumping the pinned `setuptools==` in `requirements.txt` clears it. Bump setuptools if a patched, compatible version exists; otherwise skip a low-value vendored finding and list it.
5. **System / distro Python** — `Path` under `/usr/lib/python3/dist-packages/...` → **not** a `requirements.txt` fix; the package came from `apt`, not pip. Upgrade or remove it in the Dockerfile (`apt-get install --only-upgrade <pkg>` or `apt-get purge <pkg>`), exactly like an OS package.

---

#### JMeter / Gatling jars — DO NOT FIX

Any CVE whose `Path` is under `/root/.bzt/jmeter-taurus/` or `/root/.bzt/gatling-taurus/` is **out of scope** (classification category 1). Do **not**:
- replace or repin individual bundled jars, and
- bump `JMeter.VERSION` / `Gatling.VERSION` in `bzt/modules/jmeter.py` / `bzt/modules/gatling.py` to refresh them.

A version bump is itself a JMeter/Gatling change: it only partially clears the jar CVEs (e.g. netty would still be below the fixed `4.1.135`), carries runtime/behavioral risk that unit tests don't catch, and can trip the coverage gate. Simply **list these findings** in the report under "JMeter/Gatling — not fixed (out of scope)" and move on. (The `JarTool` constants in `bzt/modules/java/tools.py` — TestNG, JUnit, etc. — are also JMeter-side tooling; leave them alone too.)

---

#### npm direct packages (`bzt/modules/javascript.py`)

Each npm tool has a `PACKAGE_NAME` constant:

```python
class Mocha(NPMPackage):
    PACKAGE_NAME = "mocha@10.6.0"   # bump version after @

class Newman(NPMPackage):
    PACKAGE_NAME = "newman@6.2.2"   # bump version after @
```

Bump the version after `@` to the fixed version from `Fix Status`.

---

#### npm transitive dependency overrides (Dockerfile)

When a CVE is in a transitive dependency of Newman or Mocha that cannot be fixed by bumping the parent package, use npm `overrides`.

**Newman** (`/root/.bzt/newman/node_modules/`):

Find the existing `printf` block in the Dockerfile that seeds `/root/.bzt/newman/package.json`. It looks like:

```dockerfile
RUN npm install newman --prefix /tmp/newman-check --silent \
 && NEWMAN_VERSION=$(node -p "require('/tmp/newman-check/node_modules/newman/package.json').version") \
 && rm -rf /tmp/newman-check \
 && if [ "$NEWMAN_VERSION" = "6.2.2" ] && [ ! -f /root/.bzt/newman/package.json ]; then \
        echo "Applying Newman dependency overrides for CVE fix (...)" \
     && mkdir -p /root/.bzt/newman \
     && printf '{\n  "overrides": {\n    "existing-pkg": "^X.Y.Z",\n    ...\n  }\n}\n' > /root/.bzt/newman/package.json; \
    ...
    fi
```

Add the new package to the `overrides` object inside the `printf` string. Update the echo message to mention the new package.

**Mocha** (`/root/.bzt/selenium-taurus/mocha/node_modules/`):

Find the `printf` block that seeds `/root/.bzt/selenium-taurus/mocha/package.json` and add the override there.

---

#### .NET SDK bump (Dockerfile)

When a CVE is in a .NET SDK component:

1. Fetch the release metadata to find the latest 8.x SDK with security fixes:
   ```bash
   curl -s "https://dotnetcli.azureedge.net/dotnet/release-metadata/8.0/releases.json" \
     | python3 -c "
   import json,sys
   data = json.load(sys.stdin)
   latest = data['releases'][0]['sdk']
   print('version:', latest['version'])
   print('url linux-x64:', [f['url'] for f in latest['files'] if f['rid']=='linux-x64' and f['name'].endswith('.tar.gz')][0])
   print('hash:', [f['hash'] for f in latest['files'] if f['rid']=='linux-x64' and f['name'].endswith('.tar.gz')][0])
   "
   ```

2. Update the `DOTNET_URL` and `DOTNET_SHA512` values in the Dockerfile `FROM system-deps AS runtimes` stage.

3. Update ALL hardcoded SDK version strings in the `sed` commands below (the `.deps.json` metadata patch lines) to match the new SDK version number.

---

#### OS packages (Dockerfile)

When a CVE is in a system package (path is empty or in `/usr/`, `/lib/`, `/bin/`):

1. Look up the fixed package version on NVD
2. Add a targeted upgrade in the Dockerfile at the `FROM system-deps` stage, after the main `apt-get install` block:
   ```dockerfile
   # Fix <CVE-ID>: upgrade <package> to <fixed-version>
   RUN apt-get update && apt-get install -y --no-install-recommends --only-upgrade <package>=<fixed-version>
   ```
   Use `--no-install-recommends` (consistent with the rest of the Dockerfile — keeps upgrades from pulling extra recommended packages).

---

#### Ruby gems (Dockerfile)

When a CVE is in a Ruby gem (path contains `/usr/local/rbenv/`):

Install the **explicit fixed version** in the Dockerfile after the rbenv setup block — pin the version with `gem install <gem> -v <fixed-version>`, never `gem update` (which jumps to latest and is not reproducible/auditable). Add `--no-document` to skip rdoc/ri generation (smaller image, faster build):
```dockerfile
# Fix <CVE-ID>: upgrade <gem> to <fixed-version>
RUN eval "$(${RBENV_ROOT}/bin/rbenv init -)" && gem install <gem> -v <fixed-version> --no-document && rbenv rehash
```

> ⚠️ **Default-gem / cached-archive caveat (verified the hard way — see `vulnerability_history.md`):** gems that ship *with Ruby* (`net-imap`, `erb`, `json`, `psych`, `uri`, etc.) are **default/bundled gems**, and Prisma reads the OLD version from **three** places that `gem install`/`gem update` leave behind — even though `gem list` shows only the patched version:
> 1. **gemspec** — `…/specifications/<gem>-<old>.gemspec` or `…/specifications/default/<gem>-<old>.gemspec`
> 2. **lib dir** — `…/gems/<gem>-<old>`
> 3. **cached archive** — `…/cache/<gem>-<old>.gem`  ← **easily missed; this is what kept net-imap 0.5.8 flagged** even after the gemspec + lib dir were removed (build #567). `gem uninstall` refuses to remove default gems.
>
> Remove all three for the OLD version only, purge the gem cache, then **assert** the old version is gone so the build fails loudly instead of silently shipping it. Use **exact** old-version strings — never broad globs like `net-imap-0.*` or `erb-4.*`, which also match the patched version (e.g. `0.5.15`) and would delete the fix:
> ```dockerfile
> RUN eval "$(${RBENV_ROOT}/bin/rbenv init -)" && \
>     gem install net-imap -v 0.5.15 --no-document && gem install erb -v 4.0.4.1 --no-document && \
>     GEMS_DIR="$(ruby -e 'print Gem.dir')" && \
>     for OLD in net-imap-0.5.8 erb-4.0.4; do \
>         rm -rf "$GEMS_DIR/gems/$OLD" "$GEMS_DIR/specifications/$OLD.gemspec" \
>                "$GEMS_DIR/specifications/default/$OLD.gemspec"; done && \
>     rm -f "$GEMS_DIR"/cache/*.gem && rbenv rehash && \
>     if find "${RBENV_ROOT}" -name 'net-imap-0.5.8.gem*' -o -name 'erb-4.0.4.gem' -o -name 'erb-4.0.4.gemspec' | grep -q .; then \
>         echo 'ERROR: vulnerable Ruby gem still on disk' >&2; exit 1; fi
> ```
> (substitute the actual gem/version pairs). Purging `cache/*.gem` wholesale is safe — cache archives aren't needed at runtime. Always confirm in the branch scan (step 12) that the old version is no longer flagged; `Successfully installed` in the build log is NOT proof.

---

### 8. Branch and worktree setup

Print a status line before each step.

**[1/6] Determine branch name**

Check if the branch already exists:
```bash
git branch --list "CVE-fixes_$(date +%Y-%m-%d)*"
```
- If not → use `CVE-fixes_YYYY-MM-DD`
- If exists → append index: `CVE-fixes_YYYY-MM-DD-2`, `-3`, etc.

**[2/6] Verify .worktrees/ is in .gitignore**

```bash
grep -q "^\.worktrees" .gitignore || echo ".worktrees" >> .gitignore
```

**[3/6] Create git worktree**

```bash
git fetch origin master && \
git worktree add .worktrees/<branch-name> -b <branch-name> origin/master
```

All subsequent steps (fixes, tests, commit, push) run from inside `.worktrees/<branch-name>/`.

### 9. Apply all auto-fixes inside the worktree

Repeat all the fix steps from step 7 — but now operating on files inside `.worktrees/<branch-name>/` instead of the main working directory.

### 10. Run unit tests

After applying all fixes inside the worktree, run the full unit test suite. This takes 10–20 minutes — tell the user and that this is expected.

```bash
cd .worktrees/<branch-name> && python -m nose2 -s tests/unit -v
```

**If tests fail → stop. Do NOT remove the worktree. Do NOT commit.**

Report:
- Which tests failed and the error output
- The worktree path: `.worktrees/<branch-name>`
- The branch name so the user can investigate

Tell the user:
> "Tests failed. The worktree has been left at `.worktrees/<branch-name>` on branch `<branch-name>` for you to investigate. Once you've resolved the failures, you can commit and push manually from that directory."

**If tests pass → continue to the coverage gate below.**

#### Coverage gate (predict the `codecov/project` check before pushing)

The PR's `codecov/project` check uses codecov defaults (`target: auto, threshold: 0%`) — **any** net coverage decrease over `bzt/` (excluding `bzt/resources`, per `.codecov.yml`) fails it and blocks merge. Detect this locally **before** committing/pushing.

- **If no `bzt/**/*.py` file was modified** (only `Dockerfile`, `requirements.txt`) → coverage cannot change (CI measures `--source=bzt` only). Skip this gate.
- **If any `bzt/**/*.py` was modified** (e.g. the npm `PACKAGE_NAME` in `bzt/modules/javascript.py`) → measure base-vs-branch coverage in the same env and compare:
  ```bash
  # in the worktree (branch), then again in a clean origin/master checkout (base)
  coverage run --source=bzt -m nose2 -s tests/unit -v ; coverage report | tail -1
  ```
  Compare the TOTAL %. (Absolute numbers differ from CI if the local Python errors some tests — that's fine; the gate is the branch-vs-base **delta**, which reproduces locally as long as both runs use the same env.)

**If branch coverage < base coverage → the PR will be blocked.** Take action before pushing:
1. Prefer adding/extending a unit test that covers the lines/branches the change left uncovered.
2. If the drop comes from a change that collapses a previously-tested branch (e.g. making two constants equal), reconsider whether that change is even in scope — JMeter/Gatling version bumps are **not** (category 1), and that was the original cause.
3. Only push once branch coverage ≥ base coverage.

### 11. Commit and push the branch (NO PR yet)

The branch must exist on `origin` so `taurus-branch-builder` can build it. **Do not create a PR here** — the PR is created in step 13, only after the branch scan confirms a reduction.

**Stage and commit** (from inside the worktree). Stage only files that actually changed:
```bash
cd .worktrees/<branch-name> && \
git add requirements.txt Dockerfile bzt/modules/services.py bzt/modules/java/tools.py \
        bzt/modules/jmeter.py bzt/modules/gatling.py bzt/modules/javascript.py
```
(`bzt/modules/services.py` holds `PipInstaller.pip_constraints` — include it whenever a Python CVE fix required updating those constraints. Stage only the files you actually changed.)

Commit message:
```
Fix CVE vulnerabilities - <comma-separated list of packages bumped>

Auto-fixed by prisma-taurus skill.
CVEs fixed: <comma-separated CVE IDs>
```

**Push the branch:**
```bash
git push -u origin <branch-name>
```

The worktree can be removed now (the branch is on `origin`). Run this from the repo root:
```bash
git worktree remove .worktrees/<branch-name>
```

### 12. Build the branch into an image and re-scan it

Trigger `taurus-branch-builder` to build the pushed branch into a Docker image, run integration tests, and run a Prisma scan on the resulting image. This is the verification gate before any PR.

```bash
curl -s -o /dev/null -D - -X POST -u "$JENKINS_USERNAME:$JENKINS_TOKEN" \
  "https://blazect-jenkins.blazemeter.com/job/taurus-branch-builder/buildWithParameters?branch_name=<branch-name>&run_integration=true&push_docker=true&public_docker=false&publish_internal_python=false&PERFORM_PRISMA_SCAN=true"
```

Parameter rationale:
- `run_integration=true` — catches breakage unit tests can't (the Dockerfile gem/npm/apt steps and the JMeter/Gatling version bumps only run for real here).
- `push_docker=true` — pushes to the **internal** GCR registry (`us.gcr.io/verdant-bulwark-278/taurus:<branch>-<build>`); `twistcli` scans that image. **Required** for the scan.
- `public_docker=false` — **never** publish a branch image to the public registry.
- `PERFORM_PRISMA_SCAN=true` — runs `twistcli images scan --details` and prints the full vulnerability table inline in the build console.

Capture the `Location:` header (queue item URL), poll the queue item for `executable.number` to get the build number, then poll the build until `building=false` (typically **~30–40 min** with integration; ~14 min without). Tell the user the build number and that you are waiting.

```bash
curl -sL -u "$JENKINS_USERNAME:$JENKINS_TOKEN" \
  "https://blazect-jenkins.blazemeter.com/job/taurus-branch-builder/<BUILD>/api/json?tree=building,result"
```

**If `result` is `FAILURE`** → integration or the build broke. Stop, do NOT create a PR, and give the console URL:
`https://blazect-jenkins.blazemeter.com/job/taurus-branch-builder/<BUILD>/console`

### 13. Compare scan results and decide whether to PR

Fetch the build console and parse the `twistcli` table (it starts at the line `Scan results for: image us.gcr.io/...:<branch>-<build>`):
```bash
curl -sL -u "$JENKINS_USERNAME:$JENKINS_TOKEN" \
  "https://blazect-jenkins.blazemeter.com/job/taurus-branch-builder/<BUILD>/console"
```

Strip ANSI codes and count rows by severity (`critical`/`high`/`medium`/`low`). **Parse the whole table — it is large; do not truncate the byte range.**

Then do two checks:

1. **Per-fix verification** — for every package you fixed, confirm its vulnerable version is **no longer flagged** in the branch scan. If a package is still flagged at the old version (e.g. `net-imap 0.5.8`), that fix did NOT land — diagnose it (see the default-gem caveat under "Ruby gems") before proceeding.
2. **Aggregate comparison** — compare the branch counts (total + per severity) against the baseline scan from steps 4–5.

**Decision gate:**
- **Vulnerabilities went down AND no fixed package is still flagged at its old version** → proceed to create the PR (below).
- **Vulnerabilities did not improve, OR a fix silently failed to land** → do NOT create a PR. Report the comparison, the failed fixes and why, and stop. Tell the user the branch is pushed and the build number so they can decide.

**Before creating the PR — create a Jira Story for the fixes and reference its key in the PR.**

Do this **only after the decision gate passes** (never for a run that didn't reduce vulnerabilities — otherwise you leave an orphan ticket). Load the deferred Atlassian (Jira) MCP tools first, e.g. `ToolSearch` with `select:mcp__claude_ai_Atlassian_Rovo__atlassianUserInfo,mcp__claude_ai_Atlassian_Rovo__searchJiraIssuesUsingJql,mcp__claude_ai_Atlassian_Rovo__createJiraIssue,mcp__claude_ai_Atlassian_Rovo__editJiraIssue,mcp__claude_ai_Atlassian_Rovo__getTransitionsForJiraIssue,mcp__claude_ai_Atlassian_Rovo__transitionJiraIssue` (add `getAccessibleAtlassianResources` if you need the cloudId).

Site / cloudId: `perforce.atlassian.net` = `2accdbdb-9d65-4c22-b174-5d4a9d437c59`. Project `MOB` (name "R&D"), issue type `Story`.

| Field | Value |
|---|---|
| Project | `MOB` |
| Issue type | `Story` |
| Summary | generic, e.g. `Fix CVE vulnerabilities in blazemeter/taurus Docker image (YYYY-MM-DD)` |
| Description | the CVEs fixed and confirmed in the branch image (CVE ID, package, old → new, severity) + baseline→branch reduction (total X→Y, crit A→A') |
| Assignee | **the developer running the skill** — `atlassianUserInfo` accountId; never hardcode a person |
| Labels | `["ai_assisted"]` |
| **Scrum Team** (`customfield_10067`) | **required on create** — hardcoded to **Sparta**, option id `21405` |
| **Product** (`customfield_10350`) | **required on create** — hardcoded to **Blazemeter**, option id `21409` |
| Sprint (`customfield_10020`) | the team's active sprint — resolve dynamically (step 2) and set it (step 4); it is **not** auto-assigned |
| Status | transition to **In Progress** after creation |

> ⚠️ **The MOB Story create screen rejects a create that omits `customfield_10067` (Scrum Team) or `customfield_10350` (Product)** — both are mandatory and have no default. For now they are **hardcoded** to this team's values (Scrum Team = Sparta `21405`, Product = Blazemeter `21409`). If another team starts running this skill, switch these to per-run values derived from the runner's own recent MOB issue instead.

1. `atlassianUserInfo` → the runner's `accountId` (assignee).
2. **Resolve the active sprint** — read it off the runner's most recent issue currently in an open sprint (the Atlassian MCP has **no board/sprint tool** and only `read/write:jira-work` scope, so the Agile API for "active sprint" is unavailable — reading an existing issue's Sprint field is the scope-free path):
   ```
   searchJiraIssuesUsingJql
     jql:    project = MOB AND assignee = currentUser() AND sprint IN openSprints() ORDER BY updated DESC
     fields: ["customfield_10020"]
   ```
   The **active sprint** = the entry in the top result's `customfield_10020` array whose `state == "active"` → its numeric `id` (e.g. `27845` = `26-Q2-S6`).
   - *Fallback:* if the runner has no open-sprint issue, resolve via `project = MOB AND "Scrum Team" = "Sparta" AND sprint IN openSprints() ORDER BY updated DESC`. If still none, create without a sprint and flag it for manual assignment.
3. `createJiraIssue` — `projectKey=MOB`, `issueTypeName=Story`, summary, description, `assignee_account_id=<accountId>`, `additional_fields: {"labels":["ai_assisted"], "customfield_10067":{"id":"21405"}, "customfield_10350":{"id":"21409"}}`. Capture the returned key, e.g. `MOB-XXXXX`. (The Sprint field is usually not on the *create* screen — set it in the next step.)
4. **Set the sprint:** `editJiraIssue` on the new key with `fields: {"customfield_10020": <activeSprintId>}` (numeric id). If the write is rejected (some boards restrict sprint assignment via API), flag the sprint for manual assignment — don't block the PR.
5. `getTransitionsForJiraIssue` → find the **In Progress** transition id → `transitionJiraIssue`.
6. **Read the ticket back** (`customfield_10020`, `status`) to confirm the sprint landed and status is *In Progress* before moving on.

**Reference the key in the PR, not the commit.** The fix commit was already made and pushed at step 11 (before this ticket exists), so the key can't be in it — and that's fine. Put the key in the **PR title and body** only (next step). Do **not** amend or force-push the fix commit to backfill the key: leaving the verified commit untouched keeps it matching exactly the image already built and scanned by `taurus-branch-builder`.

**Before creating the PR — reconcile `vulnerability_history.md` on the SAME fix branch (so it ships in this PR, no separate PR):**

`vulnerability_history.md` is a **problem-solving reference, not a log of what happened.** Its job is to help a future run (a) fix similar vulnerabilities faster and (b) know when a temporary fix can be removed because a newer upstream version resolved the original CVE. Update it **in place** — improve the matching recipe or pattern; do **not** append a blow-by-blow "what happened today" entry.

At the end of every run, reconcile it for two things:
- **A durable lesson** — a fix that silently didn't land and its root cause, a new not-buildable category, a corrected recipe, or a non-obvious decision worth preserving. The most valuable of these are only known *after* the branch scan in this step. Fold it into the matching "Patterns" recipe (or add a new recipe), not into a dated timeline.
- **A removal condition** — if this run **pruned** a now-unneeded fix (step 7) or added a new temporary fix, record *under that recipe* what the fix is for and the upstream condition that makes it removable, so a future run can prune it confidently.

If the run applied known recipes cleanly and there is no new lesson and nothing to prune, change nothing and go straight to the PR — don't pad the file. When there *is* something to record, commit it **on the fix branch** alongside the code fixes and push, so the doc update is part of this same PR rather than a separate one:
```bash
cd .worktrees/<branch-name>   # or re-add the worktree on the fix branch if already removed
git add .claude/skills/prisma-taurus/vulnerability_history.md
git commit -m "Document <lesson> in prisma-taurus history"
git push origin <branch-name>
```

> **Caveat — the history file must exist on the base branch (`master`) for this to work.** The fix branch is cut from `origin/master`; `vulnerability_history.md` is only present there once the prisma-taurus skill itself has been merged to `master`. If the skill is not yet on `master` (e.g. still on a feature branch), the file won't be in the fix branch's worktree — in that case commit the history update on whatever branch the skill lives on instead, and note in the PR that the history doc lives elsewhere. Once the skill is merged, this caveat no longer applies and the history update rides along in the same PR every run.

**Create the PR (only when the gate passes):** include the Jira key in the title so Jira ↔ GitHub link automatically.
```bash
# gh if available; otherwise POST to the GitHub API with $GITHUB_TOKEN
gh pr create --title "<MOB-XXXXX>: CVE fixes - $(date +%Y-%m-%d)" --body-file <body.md>
```
If `gh` is not on PATH, create via the GitHub API using `$GITHUB_TOKEN` (`POST /repos/Blazemeter/taurus/pulls`).

PR body should include:
- **Jira:** `<MOB-XXXXX>` (the ticket created above).
- The branch-scan comparison: baseline vs branch image (total + per severity, with the build number).
- Table of fixes confirmed in the image: CVE ID, package, old version → new version, severity.
- Note: "Verified against the `taurus-branch-builder` image scan (build #<BUILD>) before opening — integration passed and vulnerabilities dropped from X to Y."
- Note: "The following CVEs were NOT fixed and require manual intervention (see below)."

### 14. Report manual intervention and out-of-scope items

First, list the JMeter/Gatling findings that are intentionally **not** fixed:

```
═══════════════════════════════════════════════
ℹ  JMETER / GATLING — NOT FIXED (out of scope)
═══════════════════════════════════════════════
These live in the bundled JMeter/Gatling distributions and are not remediated by this skill
(no jar repinning, no JMeter/Gatling version bump). Listed for awareness only.

<count> findings — <N critical, M high, ...>
<one line per CVE: CVE-ID | severity | package version | path>
```

Then the manual section:

```
═══════════════════════════════════════════════
⚠  MANUAL INTERVENTION REQUIRED
═══════════════════════════════════════════════
The following CVEs cannot be auto-fixed and require manual changes to the Dockerfile.
They have NOT been included in the PR above.

<for each manual CVE>
CVE: <CVE-ID> | Severity: <severity> | CVSS: <score>
Package: <package> <version>
Path: <path>
Why manual: <reason — e.g. "Roslyn version string in .deps.json metadata file">
What to do: <specific instruction — see below>
```

**For `.deps.json` / Roslyn findings:**
> The scanner flags old Roslyn version strings in `.deps.json` metadata files inside the .NET SDK. These are not real vulnerable binaries — the actual Roslyn binaries are up to date. Fix options:
> 1. Bump the .NET SDK version (already handled if a .NET SDK CVE was auto-fixed above)
> 2. Add a `sed` command in the Dockerfile to patch the version string in the specific file — see the existing `sed` pattern in the Dockerfile as a reference
> 3. Add a Prisma Cloud suppression for this CVE ID — preferred if the binary is not actually vulnerable

**For `/var/lib/dpkg/` findings:**
> The scanner flags a package version string in dpkg's status database. Fix options:
> 1. Update the actual package to the flagged version if it's available in the Ubuntu repositories
> 2. Add a `sed` command in the Dockerfile to patch `/var/lib/dpkg/status` — see the existing Firefox version patch in the Dockerfile as a reference
> 3. Add a Prisma Cloud suppression — preferred if the actual installed version is not vulnerable

### 15. Final summary

End with a complete status summary:

```
═══════════════════════════════════════════════
SUMMARY
═══════════════════════════════════════════════
Branch image scan (taurus-branch-builder #<BUILD>): integration <pass/fail>
Vulnerabilities: baseline <X> → branch <Y>  (crit A→A', high B→B', med C→C', low D→D')
Jira: <MOB-XXXXX> (Story, ai_assisted, In Progress, assigned to <runner>) <sprint set | sprint NEEDS manual assignment>

✅ Verified in the branch image and included in PR #<number>:
   - <CVE-ID>: <package> <old> → <new>
   - ...

✗ Fix attempted but did NOT land in the image (excluded from PR):
   - <CVE-ID>: <package> — <why it didn't take, e.g. default-gem version still on disk>
   - ...

ℹ  JMeter/Gatling — not fixed (out of scope):
   - <count> findings (<N crit, M high, ...>) — bundled jars, not remediated

⚠  Manual intervention required (NOT in PR):
   - <CVE-ID>: <package> — <one-line reason>
   - ...

ℹ  No fix available (monitor):
   - <CVE-ID>: <package> — fix not yet released

Next steps:
1. Review and merge PR #<number> (already verified to reduce vulnerabilities in the image)
2. After merge, Jenkins builds a new blazemeter/taurus:unstable from master
3. Re-run /prisma-taurus to confirm the fixes are reflected in the unstable scan
4. Address manual-intervention items and any fixes that didn't land
```

If the decision gate in step 13 did NOT pass, end instead with: the comparison, which fixes failed to land and why, the pushed branch name and build number, and that **no PR was created** — hand it to the user to decide.

## Researching a CVE

For each vulnerability, look up sources in this order:

1. **NVD** — `https://nvd.nist.gov/vuln/detail/<CVE-ID>`
2. **GitHub Advisory Database** — `https://github.com/advisories?query=<CVE-ID>`
3. **OSV** — `https://osv.dev/vulnerability/<CVE-ID>`
4. **Snyk** — `https://security.snyk.io/vuln/<CVE-ID>`
5. **Vendor advisory** — follow the reference link from NVD

## Reference

| What | URL |
|---|---|
| On-demand scan job (baseline scan of `unstable`) | `https://blazect-jenkins.blazemeter.com/job/prisma-cloud-ondemand-scan/` |
| All builds + params | `.../api/json?tree=builds[number,timestamp,result,building,actions[parameters[name,value]]]` |
| Taurus artifact (CSV) | `.../prisma-cloud-ondemand-scan/<BUILD>/artifact/taurus.csv` |
| Build console | `.../prisma-cloud-ondemand-scan/<BUILD>/console` |
| **Branch builder (build+integration+scan a branch)** | `https://blazect-jenkins.blazemeter.com/job/taurus-branch-builder/` |
| Branch builder trigger | `.../taurus-branch-builder/buildWithParameters?branch_name=<b>&run_integration=true&push_docker=true&public_docker=false&PERFORM_PRISMA_SCAN=true` |
| Branch builder console (twistcli scan table) | `.../taurus-branch-builder/<BUILD>/console` |
| Branch image (internal GCR) | `us.gcr.io/verdant-bulwark-278/taurus:<branch>-<build>` |
| Queue item | `https://blazect-jenkins.blazemeter.com/queue/item/<ID>/api/json` |
| Docker Hub unstable tag | `https://hub.docker.com/v2/repositories/blazemeter/taurus/tags/unstable` |
| .NET 8.0 release metadata | `https://dotnetcli.azureedge.net/dotnet/release-metadata/8.0/releases.json` |
| Vulnerability fix history | `vulnerability_history.md` (in this skill's folder) — consult this before fixing any CVE |

## taurus-branch-builder parameters

| Param | Type | Use for CVE verification |
|---|---|---|
| `branch_name` | string | the pushed fix branch |
| `run_integration` | bool | `true` — verify the fixes don't break runtime |
| `push_docker` | bool | `true` — internal GCR; required so twistcli can scan the image |
| `public_docker` | bool | `false` — never publish a branch image publicly |
| `publish_internal_python` | bool | `false` |
| `PERFORM_PRISMA_SCAN` | bool | `true` — scan the built image, results printed in the console |
