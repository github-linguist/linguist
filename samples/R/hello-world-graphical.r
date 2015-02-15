library(RGtk2)   # bindings to Gtk
w <- gtkWindowNew()
l <- gtkLabelNew("Goodbye, World!")
w$add(l)
