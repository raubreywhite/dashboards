#!/bin/bash

COMPUTER=$(cat /tmp/computer)

(
  flock -n 200 || exit 1

  source /etc/environment

  echo
  echo
  echo
  echo
  echo "****START****sykdomspuls_compartmental_influenza****"

  /usr/local/bin/Rscript /src/sykdomspuls_compartmental_influenza/RunProcess.R

  echo "****END****sykdomspuls_compartmental_influenza****"
  
) 200>/var/lock/.sykdomspuls_compartmental_influenza.exclusivelock
