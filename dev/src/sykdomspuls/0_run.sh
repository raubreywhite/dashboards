#!/bin/bash

COMPUTER=$(cat /tmp/computer)

(
  flock -n 200 || exit 1

  source /etc/environment

  echo
  echo
  echo
  echo
  echo "****START****SYKDOMSPULS****"

  /usr/local/bin/Rscript /src/sykdomspuls/RunProcess.R

  if [ $? -eq 0 ] ; then
    if [ "$COMPUTER" == "smhb" ] ; then
      echo "`date +%Y-%m-%d` `date +%H:%M:%S`/$COMPUTER/BASH/SYKDOMSPULS NCFTPPUT"
      ncftpput -R -v -u "sykdomspulsen.fhi.no|data" -p $SYKDOMSPULS_PROD sykdomspulsen.fhi.no /data/ /data_app/sykdomspuls/*.RDS
      ncftpput -R -v -u "sykdomspulsen-test.fhi.no|data" -p $SYKDOMSPULS_TEST sykdomspulsen-test.fhi.no /data/ /data_app/sykdomspuls/*.RDS

      echo "`date +%Y-%m-%d` `date +%H:%M:%S`/$COMPUTER/BASH/SYKDOMSPULS TRYING TO DELETE WEBSITE DATA TOKEN"
      /src/sykdomspuls/1_delete_website_token.sh
      echo "`date +%Y-%m-%d` `date +%H:%M:%S`/$COMPUTER/BASH/SYKDOMSPULS SUCCESFULLY DELETED????"
    else
      echo "`date +%Y-%m-%d` `date +%H:%M:%S`/$COMPUTER/BASH/SYKDOMSPULS NOT SMHB - WONT MESS WITH SERVER"
    fi
  else
    echo "`date +%Y-%m-%d` `date +%H:%M:%S`/$COMPUTER/BASH/SYKDOMSPULS NO NEW DATA FILES"
  fi

  echo "****END****SYKDOMSPULS****"

) 200>/var/lock/.sykdomspuls.exclusivelock
