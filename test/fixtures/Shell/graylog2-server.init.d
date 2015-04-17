#!/bin/bash

### BEGIN INIT INFO
# Provides:        graylog2-server
# Required-Start:  $network
# Required-Stop:   $network
# Default-Start:   2 3 4 5
# Default-Stop:    1
# Short-Description: Start Graylog2 server
### END INIT INFO

# Written by Lital Natan <litaln@gmail.com>

PREFIX=/usr
SHAREDIR=$PREFIX/share/graylog2-server
SERVER_JAR=$SHAREDIR/graylog2-server.jar
SYSLOG4J_JAR=$SHAREDIR/syslog4j-0.9.46-bin.jar
SVCNAME="graylog2-server"

CONFIG="/etc/graylog2.conf"
LOGFILE="/var/log/graylog2.log"
PIDFILE="/var/run/graylog2.pid"

start() {
    if [ ! -e $CONFIG ]; then
        echo "Config file $CONFIG does not exist"
        return 1
    fi

    echo "Starting ${SVCNAME}"
        nohup `which java` -cp $SERVER_JAR:$SYSLOG4J_JAR org.graylog2.Main \
      -p ${PIDFILE} -f ${CONFIG} > $LOGFILE 2>&1 &

    # Sleep before testing the service
    sleep 2

    graylog2_test || return 1
}

stop() {
    pid=`< $PIDFILE`
    kill $pid
    rm -f ${PIDFILE} # just in case
}

graylog2_test() {
    # Graylog2 only deletes its PID file if it hits a config error
    if [ ! -e ${PIDFILE} ]; then
        echo "Configuration error, check ${CONFIG}"
        return 1
    fi

    local pid=`cat ${PIDFILE}`

    # Graylog2 isn't running, so that means there was a problem
    if [ ! -e /proc/$pid ]; then
        echo "Something went wrong, check ${LOGFILE}"
        rm -f ${PIDFILE}
        return 1
    else
        return 0
    fi
}


status() {
    graylog2_test > /dev/null 2>&1
    if [ "$?" == "0" ]; then
       echo "Graylog2 server is up"
       return 0
    else
       echo "Graylog2 server is down"
       return 1
    fi
}


restart() {
    stop
    start
}

case "$1" in
    start)
  start
  ;;
    stop)
  stop
  ;;
    status)
        status
        ;;
    restart)
  restart
  ;;
    *)
  echo "Usage $0 {start|stop|restart|status}"
  RETVAL=1
esac
