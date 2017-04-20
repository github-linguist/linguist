#
# MicropendousX LPC17xx Development Board
#
# http://www.MicropendousX.org
#
# Connect to a debugger controlling a LPC17xx
# and download firmware.
#
# Start your OpenOCD gdb server before running
# this script with:  arm-none-eabi-gdb -x gdb_lpc17xx_program.gdb
#
# Note the 'monitor' command just passes its
# arguments to OpenOCD
#
# This file is released under the MIT License
#

# Connect to OpenOCD gdb server
target remote localhost:3333

# reset the LPC17xx IC with the OpenOCD reset command
monitor reset

# the following are OpenOCD commands as in OpenOCD_program.script
# which will download a hex file into your LPC17xx
monitor halt
monitor sleep 200
monitor wait_halt
monitor flash probe 0
monitor flash info 0
monitor flash write_image erase unlock USBtoSerial.hex
monitor sleep 200
monitor reset run
monitor exit

quit
