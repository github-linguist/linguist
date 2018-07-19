require 'curses'

Curses.init_screen
begin
  Curses.setpos(6, 3)  # column 6, row 3
  Curses.addstr("Hello")

  Curses.getch  # Wait until user presses some key.
ensure
  Curses.close_screen
end
