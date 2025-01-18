// Demonstrate the use of "circles_within_range", which is a simple function to implement
// fast collision or other algorithms that need to know about nearby objects.

import std
import vec
import color
import gl

let num_balls = 1000  // try 10000 !
let ball_size = 1.0 / sqrt(num_balls) / 3.0
let ball_speed = 0.001
let push_force = 0.2  // Needs to be balanced with speed.

let positions = map(num_balls): rnd_float2()
let radiuses = map(num_balls): (rnd_float() + 0.5) * ball_size
let dirs = map(num_balls): rnd_float2_norm()

fatal(gl.window("collision", 1024, 1024))

while gl.frame():
    if gl.button("escape") == 1: return
    gl.window_title("FPS: {(1.0 / gl.delta_time())}")
    gl.clear(color_black)
    gl.scale(float(gl.window_size().y))

    // Move balls out of their own motivation.
    for(num_balls) i:
        positions[i] += dirs[i] * ball_speed
        // Keep within bounds.
        if not positions[i].x.in_range(1.0): dirs[i] *= float2 { -1.0,  1.0 }
        if not positions[i].y.in_range(1.0): dirs[i] *= float2 {  1.0, -1.0 }

    // For each ball, find all nearby balls in an efficient manner.
    let push_indices = circles_within_range(0.0, positions, radiuses, [], [], int2_0)

    // Now use this information to push other balls away.
    for(push_indices) iv, j:
        for(iv) i:
            let v = positions[j] - positions[i]
            let dist = magnitude(v) - radiuses[i] - radiuses[j]
            positions[i] += -normalize(v) * push_force * -dist

    // Render them.
    for(positions) p, i:
        gl.translate p:
            gl.circle(radiuses[i], 20)
