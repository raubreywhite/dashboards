#!/bin/bash

COMPUTER=$(cat /tmp/computer)

(
  flock -n 200 || exit 1

  source /etc/environment

  echo
  echo
  echo
  echo
  echo "****START****NORMOMO****"

  if [ "$COMPUTER" == "smhb" ] ; then
    echo "`date +%Y-%m-%d` `date +%H:%M:%S`/$COMPUTER/BASH/NORMOMO GRAB DATA"
    sshpass -p$NORMOMO_EVRY sftp -o StrictHostKeyChecking=no -oBatchMode=no -b /src/normomo/data_grab.src FHIDOD01@sftp.infotorg.no;
    mv /data_raw/normomo/ut/* /data_raw/normomo/
    rmdir /data_raw/normomo/ut
  else
    echo "`date +%Y-%m-%d` `date +%H:%M:%S`/$COMPUTER/BASH/NORMOMO NOT SMHB - WONT GRAB DATA FROM EVRY"
  fi

  /usr/local/bin/Rscript /src/normomo/RunProcess.R

  echo "****END****NORMOMO****"
  
) 200>/var/lock/.normomo.exclusivelock
