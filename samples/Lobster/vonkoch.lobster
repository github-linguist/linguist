import color
import gl
import imgui
import vec

// Sequence of the angles for the Von Koch curve's segment. Each angle
// represent a rotation to be done after the previous segment is drawn.
let seq = [ -60, 120, -60, 0 ]

def curve(depth, d) -> void:
    if depth <= 0:
        // Draw a single segment and move to its end.
        gl.line(float2 { 0.0, 0.0 }, float2 { d, 0.0 }, 0.002)
        gl.translate(float2 { d, 0.0 })
    else:
        for (seq) angle:
            curve(depth - 1, d / 3.0)
            gl.rotate_z(sincos(angle))

fatal(gl.window("Von Koch\'s curve", 640, 640))
im.init(false)
assert im.add_font("data/fonts/Droid_Sans/DroidSans.ttf", 20.0)

class Setup:
    depth: int

var setup = Setup { 5 }

while gl.frame():
    if gl.button("escape") == 1: return
    gl.clear(color_white)
    gl.color(color_blue)
    gl.scale(gl.window_size().y * 0.75)
    gl.translate(float2 { 0.1, 0.4 })

    // Draw the Von koch's curve segments around a tringle
    for (3):
        curve(setup.depth, 1.0)
        gl.rotate_z(sincos(120.0))

    im.frame():
        im.window("Parameters", im.window_always_autoresize):
            setup = im.edit_anything(setup)

