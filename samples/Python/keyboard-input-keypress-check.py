#!/usr/bin/env python

import thread
import time


try:
    from msvcrt import getch
except ImportError:
    def getch():
        import sys, tty, termios
        fd = sys.stdin.fileno()
        old_settings = termios.tcgetattr(fd)
        try:
            tty.setraw(sys.stdin.fileno())
            ch = sys.stdin.read(1)
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        return ch

char = None

def keypress():
    global char
    char = getch()

thread.start_new_thread(keypress, ())

while True:
    if char is not None:
        print "Key pressed is " + char
        break
    print "Program is running"
    time.sleep(5)
