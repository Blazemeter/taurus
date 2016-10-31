#!/bin/bash

export CHROME_WRAPPER=$(readlink -f "$0")
exec -a "$0" /etc/alternatives/google-chrome --no-sandbox "$@"
