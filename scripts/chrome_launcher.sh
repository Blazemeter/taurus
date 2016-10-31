#!/bin/bash
#
# Copyright (c) 2011 The Chromium Authors, Blazemeter. All rights reserved.
# Use of this source code is governed by a BSD-style license.

# Let the wrapped binary know that it has been run through the wrapper.
export CHROME_WRAPPER="`readlink -f "$0"`"

HERE="`dirname "$CHROME_WRAPPER"`"

export CHROME_VERSION_EXTRA="stable"

# We don't want bug-buddy intercepting our crashes. http://crbug.com/24120
export GNOME_DISABLE_CRASH_DIALOG=SET_BY_GOOGLE_CHROME

# Automagically migrate user data directory.
# TODO(phajdan.jr): Remove along with migration code in the browser for M33.
if [[ -n "" ]]; then
  if [[ ! -d "" ]]; then
    "$HERE/chrome" "--migrate-data-dir-for-sxs=" \
      --enable-logging=stderr --log-level=0
  fi
fi

# Make sure that the profile directory specified in the environment, if any,
# overrides the default.
if [[ -n "$CHROME_USER_DATA_DIR" ]]; then
  PROFILE_DIRECTORY_FLAG="--user-data-dir=$CHROME_USER_DATA_DIR"
fi

# Sanitize std{in,out,err} because they'll be shared with untrusted child
# processes (http://crbug.com/376567).
exec < /dev/null
exec > >(exec cat)
exec 2> >(exec cat >&2)

# Note: exec -a below is a bashism.
# DOCKER SELENIUM NOTE: Strait copy of script installed by Chrome with the exception of adding
# the --no-sandbox flag here.
exec -a "$0" "$HERE/chrome" --no-sandbox "$PROFILE_DIRECTORY_FLAG" "$@"
exec -a "$0" /etc/alternatives/google-chrome --no-sandbox "$@"
