#!/bin/bash

export CHROME_WRAPPER=$(readlink -f "$0")
HERE=$(dirname "$CHROME_WRAPPER")

exec -a "$0" "$HERE/chrome" --no-sandbox "$@"
