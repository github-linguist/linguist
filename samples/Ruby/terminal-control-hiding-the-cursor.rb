require "curses"
include Curses

init_screen
begin
  curs_set(1) #visible cursor
  sleep 3
  curs_set(0) #invisible cursor
  sleep 3
  curs_set(1) #visible cursor
  sleep 3
ensure
  close_screen
end
