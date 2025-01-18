// Showing off physics features in Lobster

import vec
import color
import gl
import texture
import camera
import physics
import gl

fatal(gl.window("Physics demo : water", 1024, 768, gl.window_init_linear_color))

let worldsize = float2 { 60.0, 40.0 }

ph.initialize(float2 { 0.0, -10.0 })
ph.initialize_particles(0.15)
ph.set_shader(nil, "color_attr_particle")

let floor = ph.create_box(float2 {   0.0, 1.0 }, float2 { 20.0, 1.0 })
let wall1 = ph.create_box(float2 { -20.0, 7.0 }, float2 {  1.0, 7.0 })
let wall2 = ph.create_box(float2 {  20.0, 7.0 }, float2 {  1.0, 7.0 })

floor.ph.set_color(color_dark_grey)
wall1.ph.set_color(color_dark_grey)
wall2.ph.set_color(color_dark_grey)

let boxes = map 5:
    let b = ph.create_box(float2 { 0.0, 10.0 + _ * 3.0 }, float2 { 1.0, 1.0 })
    b.ph.dynamic(true)
    b

ph.create_particle_circle(float2 { 0.0, 5.0 }, 7.0, color_red, ph.colormixingparticle)

while gl.frame() and gl.button("escape") != 1:
    gl.clear(color_black)

    // create right-handed coordinate system, with (0, 0) at the bottom middle
    set_2d_worldspace(worldsize, float2 { 0.5, 1.0 }, float2 { 1.0, -1.0 })

    if gl.button("mouse1") >= 1:
        ph.create_particle_circle(gl.local_mouse_pos(0), 0.5, color_blue, ph.colormixingparticle)

    ph.step(gl.delta_time(), 8, 3)

    gl.blend(blend_add_alpha)
    ph.render_particles(2.5)
    gl.blend(blend_none)
    ph.render()
