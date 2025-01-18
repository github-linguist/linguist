// graphics demo showing Lobster's built-in loading of (animated) .iqm models

import vec
import color
import gl
import texture
import camera
import gl
import mg

let vrmode = vr.init()

fatal(gl.window(if vrmode: "VR Test" else: "No VR device", 1280, 640,
                if vrmode: gl.window_init_no_vsync else: 0, 16 /*samples*/))

// Make a quick sample scene out of some trees.

def model_tree(numb, branchl, narrowf, leafs, leafc) -> void:
    if numb:
        mg.translate(float3_z * branchl):
            mg.tapered_cylinder(1.0, narrowf, branchl)
            mg.translate(float3_z * branchl):
                let axis = sincos(rnd(360))
                let branches = rnd(3) + 1
                for(branches) i:
                    mg.rotate(float3_z, 360 * i / branches):
                        mg.rotate(float3(axis, 0.0), 12 * branches):
                            mg.scale(narrowf):
                                mg.sphere(1.0)
                                model_tree(numb - 1, branchl, narrowf, leafs, leafc)
    else:
        mg.color(leafc):
            mg.sphere(leafs)

mg.smooth(0.5)
mg.color(color { 0.7, 0.6, 0.5, 1.0 })
model_tree(10, 1.5, 0.75, 12, color { 0.6, 1.0, 0.6, 1.0 })
let tree = mg.polygonize(50)

mg.smooth(0.0)
mg.color(color_red)
mg.sphere(1.0)
let sphere = mg.polygonize(3)

let camera = Camera { float3 { 0.0, 0.0, 3.0 }, 135.0, 20.0 }

let balls = []::[float3]

let controller_meshes = map(10): nil

def drawworld():
    gl.clear(color_grey)
    gl.blend(blend_none)

    gl.light(camera.position, float2 { 64.0, 1.0 })
    //gl.light(float3(sincos(gl.time() * 20), 0) * 100 + float3_z * 100)

    gl.set_shader("flat")
    for(10) i:
        gl.translate sincos(i * 72.0) * 3.0:
            gl.scale(0.3):
                gl.render_mesh(tree)

    for(balls) b:
        gl.translate b[0]:
            gl.scale(0.1):
                gl.rotate_z(sincos(gl.time() * 30.0)):
                    gl.render_mesh(sphere)

    gl.set_shader("color")
    gl.debug_grid(int3 { 20, 20, 0 }, float3_1, 0.005)

while gl.frame():
    if gl.button("escape") == 1: return

    let znear = 0.1
    let zfar = 1000
    if vrmode:
        vr.start()
        for(2) e:
            vr.start_eye(e, znear, zfar)
            set_z_up_worldspace():
                drawworld()
            for(vr.num_motion_controllers()) mc:
                if vr.motioncontrollerstracking(mc):
                    var mcmesh = controller_meshes[mc]
                    if not mcmesh:
                        // FIXME: sadly have to do this on the fly because at init controllers and
                        // meshes are not available for some reason.
                        controller_meshes[mc] = mcmesh = vr.create_motion_controller_mesh(mc)
                    if mcmesh:
                        gl.color(color_white)
                        gl.push_model_view()
                        vr.motion_controller(mc)
                        gl.set_shader("phong_textured")
                        gl.render_mesh(mcmesh)
                        gl.pop_model_view()
        vr.finish()
        for(2) mc:
            if vr.motion_controller_button(mc, "trigger") == 1:
                let pos = to_z_up(vr.motion_controller_vec(mc, 3))
                let dir = to_z_up(vr.motion_controller_vec(mc, 2))
                balls.push([ pos, -dir ])
        // Render the two eye textures to the non-VR window for feedback:
        gl.clear(color_black)
        gl.blend(blend_none)
        gl.set_shader("textured")

        for(2) e:
            vr.set_eye_texture(0, e)
            gl.translate(float2 { gl.window_size().x * e / 2.0, gl.window_size().y }):
                    gl.rect(float2 { gl.window_size().x / 2.0, -gl.window_size().y })
    else:
        // This is how we'd render normally, with no HMD connected:
        gl.cursor(false)
        camera.FPS_update("w", "a", "s", "d", 10.0, 4.0, true)
        gl.clear(color_dark_grey)
        gl.blend(blend_none)
        gl.perspective(70.0, znear, zfar)
        camera.FPS_view()
        drawworld()

    if balls.length > 100:
        balls.remove_range(0, balls.length - 100)
    for(balls) b:
        b[0] += b[1] * gl.delta_time() / 10.0
