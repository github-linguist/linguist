require"iuplua"
l = iup.label{title="There have been no clicks yet."}
b = iup.button{title="Click me!"}
clicks = 0
function b:button_cb()
  clicks = clicks + 1
  l.title = "There have been " .. clicks/2 .. " clicks so far." --yes, this is correct.
end
dlg = iup.dialog{iup.vbox{l, b}, title="Simple Windowed Application"}
dlg:show()

if (not iup.MainLoopLevel or iup.MainLoopLevel()==0) then
  iup.MainLoop()
end
