#!/bin/bash

WRAPPER=$(readlink -f "$0")
HERE=$(dirname "$WRAPPER")

"$HERE/_google-chrome" --no-sandbox "$@"
