// demonstrates how Lobster's event state can help program common operations without having to store state yourself

import vec
import color
import gl

fatal(gl.window("event animation and mouse drag test", 1024, 1024))

let lines = []

while gl.frame():

    if gl.button("escape") == 1: return

    gl.clear(color_black)
    gl.color(color_white)

    // allow user to drag a line, and store it when complete
    // note how we can track a drag operation conveniently thanks to gl.last_pos
    let start = float(gl.last_pos("mouse1", true))
    let cur = float(gl.mouse_pos(0))
    if magnitude(start - cur) > 1.0:  // a click is not a drag
        let down, up = gl.button("mouse1")
        if down >= 1:      // show feedback
            gl.line(start, cur, 1.0)
        if up == 1:      // store line
            lines.push([start, float(gl.last_pos("mouse1", false))])
            // note how we use the mouse up position, which may be != cur

    // draw previous lines
    gl.color(color_light_grey)
    for(lines) l:
        gl.line(l[0], l[1], 1.0)

    // just for demonstration purposes, gl.last_time makes it easy to animate clicks etc.
    // we show little animated circles on each mouse down or up
    for(2) down:
        gl.color(if down: color_green else: color_blue)
        let clicktime = gl.time() - gl.last_time("mouse1", down)
        if  clicktime < 1.0:
            gl.translate gl.last_pos("mouse1", down):
                gl.line_mode 1:
                    gl.circle(clicktime * 20.0, 10)
