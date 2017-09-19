#!/bin/bash

echo "MSIS"
echo "$COMPUTER"

(
  flock -n 200 || exit 1

  source /etc/environment

  /usr/local/bin/Rscript /src/msis/RunProcess.R


) 200>/var/lock/.msis.exclusivelock
