#!/bin/bash
#
# Copyright (c) 2011 The Chromium Authors, Taurus project. All rights reserved.
# Use of this source code is governed by a BSD-style license.

export CHROME_WRAPPER="`readlink -f "$0"`"
HERE="`dirname "$CHROME_WRAPPER"`"

exec -a "$0" "$HERE/chrome" --no-sandbox "$@"
exec -a "$0" /etc/alternatives/google-chrome --no-sandbox "$@"
