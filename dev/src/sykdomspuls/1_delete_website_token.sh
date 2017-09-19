source /etc/environment

HOST='sykdomspulsen.fhi.no'
USER='sykdomspulsen.fhi.no|data'
PASSWD=$SYKDOMSPULS_PROD

ftp -n $HOST <<END_SCRIPT
quote USER $USER
quote PASS $PASSWD
cd data
del .deleteThisToRestartR
quit
END_SCRIPT
exit 0

HOST='sykdomspulsen-test.fhi.no'
USER='sykdomspulsen-test.fhi.no|data'
PASSWD=$SYKDOMSPULS_TEST

ftp -n $HOST <<END_SCRIPT
quote USER $USER
quote PASS $PASSWD
cd data
del .deleteThisToRestartR
quit
END_SCRIPT
exit 0
