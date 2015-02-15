import
  gtk2, strutils, glib2

var valu: int = 0
var chngd_txt_hndler: gulong = 0

proc thisCheckBtns   # forward declaration

proc thisDestroy(widget: pWidget, data: pgpointer){.cdecl.} =
  main_quit()

nimrod_init()
var window = window_new(gtk2.WINDOW_TOPLEVEL)
var content = vbox_new(TRUE,10)
var hbox1 = hbox_new(TRUE,10)
var entry_fld = entry_new()
entry_fld.set_text("0")
var btn_quit = button_new("Quit")
var btn_inc = button_new("Increment")
var btn_dec = button_new("Decrement")
add(hbox1,btn_inc)
add(hbox1,btn_dec)
pack_start(content, entry_fld, TRUE, TRUE, 0)
pack_start(content, hbox1, TRUE, TRUE, 0)
pack_start(content, btn_quit, TRUE, TRUE, 0)
set_border_width(Window, 5)
add(window, content)

proc thisInc(widget: pWidget, data: pgpointer){.cdecl.} =
  inc(valu)
  entry_fld.set_text($valu)
  thisCheckBtns()

proc thisDec(widget: pWidget, data: pgpointer){.cdecl.} =
  dec(valu)
  entry_fld.set_text($valu)
  thisCheckBtns()

proc thisTextChanged(widget: pWidget, data: pgpointer) {.cdecl.} =
  #signal_handler_block(entry_fld, chngd_txt_hndler)
  try:
     valu = parseInt($entry_fld.get_text())
  except EInvalidValue:
     valu = 0
  entry_fld.set_text($valu)
  #signal_handler_unblock(entry_fld, chngd_txt_hndler)
  #signal_emit_stop(entry_fld, signal_lookup("changed",TYPE_EDITABLE()),0)
  thisCheckBtns()

proc thisCheckBtns =
   set_sensitive(btn_inc, valu < 10)
   set_sensitive(btn_dec, valu > 0)
   set_sensitive(entry_fld, valu == 0)

discard signal_connect(window, "destroy",
                   SIGNAL_FUNC(thisDestroy), nil)
discard signal_connect(btn_quit, "clicked",
                   SIGNAL_FUNC(thisDestroy), nil)
discard signal_connect(btn_inc, "clicked",
                   SIGNAL_FUNC(thisInc), nil)
discard signal_connect(btn_dec, "clicked",
                   SIGNAL_FUNC(thisDec), nil)
chngd_txt_hndler = signal_connect(entry_fld, "changed",
                   SIGNAL_FUNC(thisTextChanged), nil)

show_all(window)
thisCheckBtns()
main()
