local gl = require "luagl"
local iup = require "iuplua"
require "iupluagl"

local function paint()
  gl.ClearColor(0.3,0.3,0.3,0.0)
  gl.Clear"COLOR_BUFFER_BIT,DEPTH_BUFFER_BIT"

  gl.ShadeModel"SMOOTH"

  gl.LoadIdentity()
  gl.Translate(-15.0, -15.0, 0.0)

  gl.Begin"TRIANGLES"
  gl.Color(1.0, 0.0, 0.0)
  gl.Vertex(0.0, 0.0)
  gl.Color(0.0, 1.0, 0.0)
  gl.Vertex(30.0, 0.0)
  gl.Color(0.0, 0.0, 1.0)
  gl.Vertex(0.0, 30.0)
  gl.End()

  gl.Flush()
end

local function reshape(width, height)
  gl.Viewport(0, 0, width, height)
  gl.MatrixMode"PROJECTION"
  gl.LoadIdentity()
  gl.Ortho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)
  gl.MatrixMode"MODELVIEW"
end

local glc = iup.glcanvas{rastersize="640x480"}
function glc:action() paint() end
function glc:resize_cb(w,h) reshape(w,h) end
function glc:map_cb() iup.GLMakeCurrent(self) end

local dlg = iup.dialog{title="Triangle", shrink="yes"; glc}
dlg:show()

iup.MainLoop()
