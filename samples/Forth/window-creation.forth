include ffl/gsv.fs

\ Open the connection to the gtk-server and load the Gtk2 definitions
s" gtk-server.cfg" s" ffl-fifo" gsv+open 0= [IF]

\ Convert the string event to a widget id
: event>widget
  0. 2swap >number 2drop d>s
;

0 value window

: window-creation
  gtk_init

  \ Create the window
  GTK_WINDOW_TOPLEVEL gtk_window_new to window

  window gtk_widget_show

  \ Wait for an event
  BEGIN
    s" WAIT" gtk_server_callback
    event>widget window =
  UNTIL

  0 gtk_exit
;

window-creation

gsv+close drop
[THEN]
