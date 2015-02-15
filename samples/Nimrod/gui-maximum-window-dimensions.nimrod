import
  gtk2, gdk2

nimrod_init()
var w = gdk2.screen_width()
var h = gdk2.screen_height()
echo("WxH=",w,"x",h)
