import os, strutils

echo("Enter how long I should sleep (in milliseconds):")
var timed = parseInt(readLine(stdin).string)
echo("Sleeping...")
sleep(timed)
echo("Awake!")
