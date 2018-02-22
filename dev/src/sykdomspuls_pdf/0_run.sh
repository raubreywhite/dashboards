#!/bin/bash

COMPUTER=$(cat /tmp/computer)

(
  flock -n 200 || exit 1

  source /etc/environment

  echo
  echo
  echo
  echo
  echo "****START****sykdomspuls_pdf****"

  /usr/local/bin/Rscript /src/sykdomspuls_pdf/RunProcess.R

  echo "****END****sykdomspuls_pdf****"
  
) 200>/var/lock/.sykdomspuls_pdf.exclusivelock
