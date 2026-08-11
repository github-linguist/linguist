// graphics demo showing very simple link based physics, based on a processing.js example

import std
import vec
import color
import gl

class segment:
    pos:float2
    dir = float2_0

let segs = map 7: segment { float2 { 0.5, 1.0 } }

let ballradius = 0.1
var ball = float2_h
var ballvel = float2 { 0.005, -0.004 }

fatal(gl.window("reach", 600, 600))

while gl.frame():
    if gl.button("escape") == 1: return
    gl.clear(color_black)

    gl.scale(min(gl.window_size().x, gl.window_size().y))

    ball += ballvel

    if(ball.x > 1.0 - ballradius or ball.x < ballradius): ballvel *= float2 { -1.0,  1.0 }
    if(ball.y > 1.0 - ballradius or ball.y < ballradius): ballvel *= float2 {  1.0, -1.0 }

    gl.translate ball:
        gl.color(color_dark_grey)
        gl.circle(ballradius, 50)
        gl.color(color_light_grey)
        gl.circle(ballradius / 5.0, 20)

    var target = float2_0
    let seglength = 0.1

    for(segs) seg, i:
        let in = if i: target else: ball
        seg.dir = normalize(in - seg.pos) * seglength
        target = in - seg.dir

    reduce_reverse(segs) b, a:
        b.pos = a.pos + a.dir
        b

    for(segs) seg, i:
        gl.translate seg.pos:
            let segwidth = 0.01 + i * 0.005
            gl.color(color_white)
            gl.line(float2_0, seg.dir, segwidth)
            gl.translate(seg.dir)
            gl.color(color_grey)
            gl.circle(segwidth / 2.0 + 0.005, 20)

