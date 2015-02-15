local iup = require "iuplua"

iup.dialog{
  title = "Window";
  iup.vbox{
    margin = "10x10";
    iup.label{title = "A window"}
  }
}:show()

iup.MainLoop()
