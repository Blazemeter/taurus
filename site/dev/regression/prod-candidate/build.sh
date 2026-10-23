#!/bin/bash -e
# Builds the production image (site/Dockerfile) from the working tree the way site/deploy-site.sh does
# for a release, without touching the repository: the site is copied to a temporary directory first.
# Differences to deploy-site.sh: learn/ is taken from site/legacy/learn (legacy/fetch-learn.sh downloads
# the same learn.zip from GCS) and the image is tagged taurus-site:prod-candidate instead of being pushed.
DIR=$(cd "$(dirname "$0")" && pwd)
SITE=$(cd "$DIR/../../.." && pwd)
CTX=$(mktemp -d "${TMPDIR:-/tmp}/taurus-site-candidate.XXXXXX")
trap 'rm -rf "$CTX"' EXIT

[ -d "$SITE/legacy/learn" ] || { echo "missing $SITE/legacy/learn, run site/legacy/fetch-learn.sh first"; exit 1; }

# site sources as in the git checkout used by deploy-site.sh, local playgrounds and installed vendor excluded
rsync -a --exclude /dev --exclude /legacy --exclude /vendor "$SITE/" "$CTX/site/"

# deploy-site.sh steps for a release build (UNSTABLE_SNAPSHOT replaced with nothing, empty builds/)
mkdir -p "$CTX/site/builds"
perl -pi -e 's/UNSTABLE_SNAPSHOT//' "$CTX/site/dat/docs/Installation.md"
docker run --rm -v "$CTX/site:/site" -w /site python:3.12-slim sh -c \
    'pip install -q --no-cache-dir pyyaml==6.0.2 2>/dev/null; python3 Taurus/kwindexer.py dat/docs dat/docs/KeywordIndex.md 2>/dev/null'
cp -R "$SITE/legacy/learn" "$CTX/site/learn"

docker build --platform linux/amd64 -t taurus-site:prod-candidate "$CTX/site"
