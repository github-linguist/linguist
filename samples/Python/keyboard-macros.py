#!/usr/bin/env python
import curses

def print_message():
    stdscr.addstr('This is the message.\n')

stdscr = curses.initscr()
curses.noecho()
curses.cbreak()
stdscr.keypad(1)

stdscr.addstr('CTRL+P for message or q to quit.\n')
while True:
    c = stdscr.getch()
    if c == 16: print_message()
    elif c == ord('q'): break

curses.nocbreak()
stdscr.keypad(0)
curses.echo()
curses.endwin()
