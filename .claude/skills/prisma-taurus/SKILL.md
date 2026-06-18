---
name: prisma-taurus
description: Use when the user wants to check, fix, or verify Prisma Cloud vulnerability scan results for the Taurus Docker image (blazemeter/taurus:unstable).
---

## Overview

End-to-end vulnerability management for the Taurus Docker image. Fetches the latest Prisma Cloud scan, classifies findings into auto-fixable and manual categories, applies all safe fixes, runs unit tests, and opens a PR.

## When to Use

- User asks about Taurus CVEs, vulnerabilities, or security scan results
- User wants to fix vulnerabilities found in `blazemeter/taurus`
- User wants to verify that a previous fix resolved a CVE
- User asks to run or check the Prisma scan for Taurus

## Common Mistakes

- Triggering a new scan when a valid one already exists (within 5 days and after last `unstable` push) — always check first
- Auto-fixing scanner appeasement CVEs (`.deps.json`, `/var/lib/dpkg/` paths) — these must always be flagged as manual
- Removing the worktree after a test failure — leave it in place for the user to investigate
- Re-scanning after PR creation — there is no PR Docker image for Taurus; re-scan only after the PR is merged and Jenkins builds a new `unstable`

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

## Fix classification rules

Before applying any fix, classify each CVE:

**Scanner appeasement → flag as manual, never auto-fix:**
- `Path` contains `/usr/share/dotnet/sdk/` (Roslyn / .deps.json metadata patches)
- `Path` contains `/var/lib/dpkg/` (OS package version string manipulation)

**Auto-fixable → apply automatically (all others where `Fix Status` starts with `fixed in`):**

| Path pattern | Fix type | Where to fix |
|---|---|---|
| `/root/.bzt/jmeter-taurus/*/lib/*.jar` | Java jar version constant | `bzt/modules/jmeter.py` or `bzt/modules/java/tools.py` |
| `/root/.bzt/gatling-taurus/*/lib/*.jar` | Java jar version constant | `bzt/modules/gatling.py` |
| `/root/.bzt/newman/node_modules/` | npm override | Dockerfile newman `package.json` printf block |
| `/root/.bzt/selenium-taurus/mocha/node_modules/` | npm override | Dockerfile mocha `package.json` printf block |
| `/root/.bzt/selenium-taurus/*/node_modules/` | npm direct package | `bzt/modules/javascript.py` PACKAGE_NAME constant |
| `/usr/local/rbenv/` or `/usr/local/lib/ruby/` | Ruby gem | Dockerfile `gem update` line |
| empty path or OS path (`/usr/lib/`, `/lib/`) | OS package | Dockerfile `apt-get install --only-upgrade` |
| Python dist-info path | Python package | `requirements.txt` |

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
- Scan timestamp is **after** the `unstable` image push timestamp
- Scan timestamp is **within the last 5 days**

If valid → skip to step 4 using that build number.

Otherwise → continue to step 3.

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
- Fix Type (`auto-fix` / `manual` / `no fix available`)

Show a summary line:
> `X vulnerabilities — Y critical, Z high, N medium, M low — A auto-fixable, B manual, C no fix available`

### 6. Collect auto-fixable CVEs

Gather all CVEs where:
- `Fix Status` starts with `fixed in`
- Path does NOT match scanner appeasement patterns

If there are no auto-fixable CVEs → report that and list any manual items, then stop.

### 7. Apply all auto-fixes

Work in priority order: critical → high → medium → low. For each CVE apply the fix according to the classification table above.

---

#### Python packages (`requirements.txt`)

Identify the package by matching `Source Package` against entries in `requirements.txt`. Update the pinned version:
```
# before
urllib3==2.6.3
# after
urllib3==2.7.0
```

Extract the fixed version from the `Fix Status` field (e.g. `fixed in 2.7.0` → `2.7.0`).

---

#### Java jar version constants

**For JMeter jars** (`/root/.bzt/jmeter-taurus/*/lib/`):

First check if bumping the JMeter version itself would bring in the fixed jar. Look up the jar in `bzt/modules/java/tools.py` or `bzt/modules/jmeter.py` — each bundled jar has a version constant:

```python
# bzt/modules/java/tools.py examples
class TestNG(JarTool):
    VERSION = "7.10.2"   # bump this

class JUnitJupiterApi(JarTool):
    VERSION = "5.10.3"   # bump this
```

```python
# bzt/modules/jmeter.py examples
PLUGINS_MANAGER_VERSION = "1.11"   # bump this
VERSION = "5.5"                    # bump JMeter itself if needed
VERSION_LATEST = "5.6.3"
```

Steps:
1. Search for the jar's artifact ID in `bzt/modules/java/tools.py` and `bzt/modules/jmeter.py`
2. Look up on NVD (`https://nvd.nist.gov/vuln/detail/<CVE-ID>`) to confirm the safe version
3. Verify the fixed version exists on Maven Central before updating
4. Update the version constant

**For Gatling jars** (`/root/.bzt/gatling-taurus/*/lib/`):

Search `bzt/modules/gatling.py` for the version constant. Bump it to the fixed version.

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
   RUN apt-get update && apt-get install -y --only-upgrade <package>=<fixed-version>
   ```

---

#### Ruby gems (Dockerfile)

When a CVE is in a Ruby gem (path contains `/usr/local/rbenv/`):

Add a targeted gem update in the Dockerfile after the rbenv setup block:
```dockerfile
# Fix <CVE-ID>: upgrade <gem> to <fixed-version>
RUN gem update <gem>
```

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

**If tests pass → continue to step 11.**

### 11. Commit, push, and PR

**[4/6] Stage and commit** (from inside the worktree)

Stage only files that were modified:
```bash
cd .worktrees/<branch-name> && \
git add requirements.txt Dockerfile bzt/modules/java/tools.py bzt/modules/jmeter.py \
        bzt/modules/gatling.py bzt/modules/javascript.py
```
(only add files that actually changed)

Commit message:
```
Fix CVE vulnerabilities - <comma-separated list of packages bumped>

Auto-fixed by prisma-taurus skill.
CVEs fixed: <comma-separated CVE IDs>
```

**[5/6] Push branch**
```bash
git push origin <branch-name>
```

**[6/6] Remove worktree and create PR**
```bash
cd /Users/rguevara/perforce/taurus && \
git worktree remove .worktrees/<branch-name>
```

Then create the PR:
```bash
gh pr create \
  --title "CVE fixes - $(date +%Y-%m-%d)" \
  --body "..."
```

PR body should include:
- Table of auto-fixed CVEs: CVE ID, package, old version → new version, severity
- Note: "Re-run `/prisma-taurus` after this PR is merged and Jenkins builds a new `unstable` image to verify fixes."
- Note: "The following CVEs were NOT fixed by this skill and require manual intervention (see below)."

Use `GITHUB_TOKEN` for authentication if `gh` is not already authenticated.

### 10. Report manual intervention items

After the PR is created, display a clear section:

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

### 11. Final summary

End with a complete status summary:

```
═══════════════════════════════════════════════
SUMMARY
═══════════════════════════════════════════════
✅ Auto-fixed and included in PR #<number>:
   - <CVE-ID>: <package> <old> → <new>
   - ...

⚠  Manual intervention required (NOT in PR):
   - <CVE-ID>: <package> — <one-line reason>
   - ...

ℹ  No fix available (monitor):
   - <CVE-ID>: <package> — fix not yet released

Next steps:
1. Review and merge PR #<number>
2. Wait for Jenkins to build a new blazemeter/taurus:unstable from master
3. Re-run /prisma-taurus to verify the auto-fixed CVEs are resolved
4. Address manual intervention items separately
```

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
| On-demand scan job | `https://blazect-jenkins.blazemeter.com/job/prisma-cloud-ondemand-scan/` |
| All builds + params | `.../api/json?tree=builds[number,timestamp,result,building,actions[parameters[name,value]]]` |
| Taurus artifact (CSV) | `.../prisma-cloud-ondemand-scan/<BUILD>/artifact/taurus.csv` |
| Build console | `.../prisma-cloud-ondemand-scan/<BUILD>/console` |
| Queue item | `https://blazect-jenkins.blazemeter.com/queue/item/<ID>/api/json` |
| Docker Hub unstable tag | `https://hub.docker.com/v2/repositories/blazemeter/taurus/tags/unstable` |
| .NET 8.0 release metadata | `https://dotnetcli.azureedge.net/dotnet/release-metadata/8.0/releases.json` |
| Vulnerability fix history | `.claude/vulnerabilities_history/vulnerability_history.md` — consult this before fixing any CVE |
