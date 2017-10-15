#!/bin/bash

WRAPPER=$(readlink -f "$0")
HERE=$(dirname "$WRAPPER")

exec "$HERE/_google-chrome" --no-sandbox --disable-infobars --start-fullscreen "$@"
