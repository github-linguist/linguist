require "iuplua"

dlg = iup.dialog{iup.label{title="Goodbye, World!"}; title="test"}
dlg:show()

if (not iup.MainLoopLevel or iup.MainLoopLevel()==0) then
  iup.MainLoop()
end
