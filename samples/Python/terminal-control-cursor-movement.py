import curses

scr = curses.initscr()
#     Demonstrate how to move the cursor one position to the left
def move_left():
	y,x = curses.getyx()
	curses.move(y,x-1)
	
#    Demonstrate how to move the cursor one position to the right
def move_right():
	y,x = curses.getyx()
	curses.move(y,x+1)
	
#    Demonstrate how to move the cursor up one line (without affecting its horizontal position)
def move_up():
	y,x = curses.getyx()
	curses.move(y-1,x)
	
#    Demonstrate how to move the cursor down one line (without affecting its horizontal position)
def move_down():
	y,x = curses.getyx()
	curses.move(y+1,x)

#    Demonstrate how to move the cursor to the beginning of the line
def move_line_home()	
	y,x = curses.getyx()
	curses.move(y,0)

#    Demonstrate how to move the cursor to the end of the line
def move_line_end()	
	y,x = curses.getyx()
	maxy,maxx = scr.getmaxyx()
	curses.move(y,maxx)

#    Demonstrate how to move the cursor to the top left corner of the screen
def move_page_home():
	curses.move(0,0)
	
#    Demonstrate how to move the cursor to the bottom right corner of the screen
def move_page_end():
	y,x = scr.getmaxyx()
	curses.move(y,x)
