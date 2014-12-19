#!/bin/bash
echo "Content-type: text/html"
day=`echo "$QUERY_STRING" | sed -n 's/^.*day=\([^&]*\).*$/\1/p' | sed "s/%20/ /g"`
month=`echo "$QUERY_STRING" | sed -n 's/^.*month=\([^&]*\).*$/\1/p' | sed "s/%20/ /g"`
year=`echo "$QUERY_STRING" | sed -n 's/^.*year=\([^&]*\).*$/\1/p' | sed "s/%20/ /g"`
hour=`echo "$QUERY_STRING" | sed -n 's/^.*hour=\([^&]*\).*$/\1/p' | sed "s/%20/ /g"`
minute=`echo "$QUERY_STRING" | sed -n 's/^.*minute=\([^&]*\).*$/\1/p' | sed "s/%20/ /g"`
second=`echo "$QUERY_STRING" | sed -n 's/^.*second=\([^&]*\).*$/\1/p' | sed "s/%20/ /g"`
echo ""
echo "<html><body>"

echo "<pre> $(killall ems) </pre>"



echo "<pre> $(date $month$day$hour$minute$year.$second) </pre>"

echo "<pre> $(/sbin/hwclock -w>/dev/null & /sbin/reboot) </pre>"

echo "<pre> $(/sbin/reboot) </pre>"






echo "</body></html>"