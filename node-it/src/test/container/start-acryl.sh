#!/bin/bash

trap 'kill -TERM $PID' TERM INT
echo Options: $ACRYL_OPTS
java $ACRYL_OPTS -cp "/opt/acryl/lib/*" com.acrylplatform.Application /opt/acryl/template.conf &
PID=$!
wait $PID
trap - TERM INT
wait $PID
EXIT_STATUS=$?
