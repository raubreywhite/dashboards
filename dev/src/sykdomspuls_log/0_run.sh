#!/bin/bash

COMPUTER=$(cat /tmp/computer)

(
  flock -n 200 || exit 1

  source /etc/environment

  echo
  echo
  echo
  echo
  echo "****START****sykdomspuls_log****"

  if [ "$COMPUTER" == "smhb" ] ; then
    echo "`date +%Y-%m-%d` `date +%H:%M:%S`/$COMPUTER/BASH/sykdomspuls_log GRAB DATA"
    ncftpget -R -v -u "sykdomspulsen.fhi.no|data" -p $SYKDOMSPULS_PROD sykdomspulsen.fhi.no /data_raw/sykdomspuls_log/ /data/log/*
  fi

  /usr/local/bin/Rscript /src/sykdomspuls_log/RunProcess.R

  echo "****END****sykdomspuls_log****"
  
) 200>/var/lock/.sykdomspuls_log.exclusivelock
