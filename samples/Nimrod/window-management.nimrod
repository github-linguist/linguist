import
  gdk2, glib2, gtk2,
  os

proc thisDestroy(widget: pWidget, data: pgpointer){.cdecl.} =
  main_quit()
proc thisMax(widget: pWidget, data: pgpointer){.cdecl.} =
  maximize(get_parent_window(widget))
proc thisUnmax(widget: pWidget, data: pgpointer){.cdecl.} =
  unmaximize(get_parent_window(widget))
proc thisIcon(widget: pWidget, data: pgpointer){.cdecl.} =
  iconify(get_parent_window(widget))
proc thisDeicon(widget: pWidget, data: pgpointer){.cdecl.} =
  deiconify(get_parent_window(widget))
proc thisHide(widget: pWidget, data: pgpointer){.cdecl.} =
  hide(get_parent_window(widget))
  sleep(5)
  show(get_parent_window(widget))

proc thisShow(widget: pWidget, data: pgpointer){.cdecl.} =
  show(get_parent_window(widget))

var isshifted: bool = false

proc thisMove(widget: pWidget, data: pgpointer){.cdecl.} =
  var w, h: gint
  get_size(get_parent_window(widget), Addr(w), Addr(h))
  if isshifted:
     move(get_parent_window(widget), w-10, h-10)
  else:
     move(get_parent_window(widget), w+10, h+10)
  isshifted = not isshifted


nimrod_init()
var window = window_new(gtk2.WINDOW_TOPLEVEL)
discard allow_grow(window)
set_title(window,"Window management")
var stackbox = vbox_new(TRUE, 10)
var bmax = button_new("maximize")
var bunmax = button_new("unmaximize")
var bicon = button_new("iconize")
var bdeicon = button_new("deiconize")
var bhide = button_new("hide")
var bshow = button_new("show")
var bmove = button_new("move")
var bquit = button_new("Quit")

pack_start(stackbox, bmax, TRUE, TRUE, 0)
pack_start(stackbox, bunmax, TRUE, TRUE, 0)
pack_start(stackbox, bicon, TRUE, TRUE, 0)
pack_start(stackbox, bdeicon, TRUE, TRUE, 0)
pack_start(stackbox, bhide, TRUE, TRUE, 0)
pack_start(stackbox, bshow, TRUE, TRUE, 0)
pack_start(stackbox, bmove, TRUE, TRUE, 0)
pack_start(stackbox, bquit, TRUE, TRUE, 0)
set_border_width(Window, 5)
add(window, stackbox)
discard signal_connect(window, "destroy",
                   SIGNAL_FUNC(thisDestroy), nil)

discard signal_connect(bicon, "clicked",
                   SIGNAL_FUNC(thisIcon), nil)
discard signal_connect(bdeicon, "clicked",
                   SIGNAL_FUNC(thisDeicon), nil)
discard signal_connect(bmax, "clicked",
                   SIGNAL_FUNC(thisMax), nil)
discard signal_connect(bunmax, "clicked",
                   SIGNAL_FUNC(thisUnmax), nil)
discard signal_connect(bhide, "clicked",
                   SIGNAL_FUNC(thisHide), nil)
discard signal_connect(bshow, "clicked",
                   SIGNAL_FUNC(thisShow), nil)
discard signal_connect(bmove, "clicked",
                   SIGNAL_FUNC(thismove), nil)
discard signal_connect(bquit, "clicked",
                   SIGNAL_FUNC(thisDestroy), nil)
show_all(window)
main()
