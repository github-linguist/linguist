// graphics demo showing Lobster's builtin facilities of generating meshes procedurally from implicit functions
// this functionality is under construction

import vec
import color
import gl
import camera
import gl
import mg

print "generating models... this may take some time!"

fatal(gl.window("meshgen test (PLEASE WAIT...)", 1280, 800, gl.window_init_linear_color))

let camera = Camera { float3_1 * 10.0, 135.0, 0.0 }

def mg.mirror_y(dist, body):
    for(2) i:
        mg.translate(float3 { 0.0, i * dist * 2.0 - dist, 0.0 }, body)

//mg.set_color_noise(0.5, 1)
let aspoints = false
mg.set_point_mode(aspoints)
if not aspoints:
    //mg.set_polygon_reduction(3, 0.98, 0.95)
    mg.set_vertex_randomize(0.0)

let density = 80

var rounded_cube = nil
if true:
    mg.smooth(1.0)
    mg.color(color_white)
    mg.rotate(float3_1, 45):
        mg.superquadric(float3_1 * 10.0, float3_1)
    rounded_cube = mg.polygonize(density)

var gun = nil
if true:
    mg.smooth(0.5)
    mg.color(color_white)
    mg.rotate(float3_y, 90.0):
        mg.scale_vec(float3 { 1.0, 1.3, 1.0 }):
            mg.supertoroid(3.0, float3 { 3.0, 3.0, 5.0 })
    mg.translate(float3 { 0.0, -1.3, 2.0 }):
        mg.rotate(float3_x, 20.0):
            mg.superquadric(float3 { 3.0, 3.0, 3.0 }, float3 { 0.5, 0.5, 3.5 })
    mg.translate(float3 { 0.0, -8.0, 4.0 }):
        mg.rotate(float3_x, 30.0):
            mg.superquadric(float3 { 3.0, 3.0, 3.0 }, float3 { 0.5, 0.5, 2.5 })
    mg.translate(float3 { 0.0, -5.5, -3.0 }):
        mg.rotate(float3_x, -20.0):
            mg.superquadric(float3 { 5.0, 5.0, 100.0 }, float3 { 1.5, 2.5, 6.5 })
    mg.translate(float3 { 0.0, 2.0, 4.2 }):
        mg.superquadric(float3 { 3.0, 100.0, 3.0 }, float3 { 2.0, 10.0, 3.0 })
    mg.translate(float3 { 0.0, 14.0, 5.0 }):
        mg.rotate(float3_x, 90.0):
            mg.cylinder(1.0, 3.0)
    gun = mg.polygonize(density)

var spaceship = nil
if true:
    mg.smooth(0.5)
    mg.color(color_white)
    mg.superquadric_non_uniform(float3 { 1.0, 2.0, 2.0 }, float3 { 2.0, 2.0, 2.0 },
                                float3 { 2.0, 3.5, 0.3 }, float3 { 0.5, 3.5, 0.3 })
    mg.superquadric_non_uniform(float3 { 2.0, 1.5, 1.0 }, float3 { 2.0, 1.5, 2.0 },
                                float3 { 4.0, 1.0, 0.7 }, float3 { 0.7, 1.0, 0.4 })
    mg.translate(float3 { 1.0, 0.0, 0.4 }):
        mg.superquadric_non_uniform(float3 { 2.0, 2.0, 1.0 }, float3 { 2.0, 2.0, 2.0 },
                                    float3 { 1.0, 0.5, 0.4 }, float3 { 0.5, 0.5, 0.5 })
    mg.mirror_y(1.5):
        mg.translate(float3_x):
            mg.superquadric(float3 { 100.0, 2.0, 2.0 }, float3 { 1.3, 0.2, 0.2 })
    mg.mirror_y(2):
        mg.translate(float3_x):
            mg.superquadric(float3 { 100.0, 2.0, 2.0 }, float3 { 1.0, 0.15, 0.15 })
    mg.mirror_y(1):
        mg.translate(float3_x * -0.6):
            mg.superquadric_non_uniform(float3 { 1.0, 2.0, 2.0 }, float3 { 1.0, 2.0, 2.0 },
                                        float3 { 1.5, 0.5, 0.5 }, float3 { 0.01, 0.5, 0.5 })
            mg.color(color { 1.0, 1.0, 1.0, 0.0 /* carve */ }):
                mg.rotate(float3_y, 90.0):
                    mg.smooth(0.0):
                        mg.cylinder(0.35, 0.25)
    spaceship = mg.polygonize(density * 2)

def model_tree(numb, branchl, narrowf, leafs, leafc) -> void:
    if numb:
        mg.translate(float3_z * branchl):
            mg.tapered_cylinder(1.0, narrowf, branchl)
            mg.translate(float3_z * branchl):
                let axis = float3(sincos(rnd(360)), 0.0)
                let branches = rnd(3) + 1
                for(branches) i:
                    mg.rotate(float3_z, 360 * i / branches):
                        mg.rotate(axis, 12 * branches):
                            mg.scale(narrowf):
                                mg.sphere(1.0)
                                model_tree(numb - 1, branchl, narrowf, leafs, leafc)
    else:
        mg.color(leafc):
            mg.sphere(leafs)

let trees = []

if true:
    mg.smooth(0.5)
    mg.color(color { 0.6, 0.5, 0.4, 1.0 })
    model_tree(10, 1.5, 0.75, 12, color { 0.4, 1.0, 0.4, 1.0 })
    trees.push(mg.polygonize(density))

    mg.smooth(0.5)
    mg.color(color { 0.4, 0.3, 0.2, 1.0 })
    model_tree(10, 1.2, 0.8, 1.2, color { 0.6, 1.0, 0.3, 1.0 })
    trees.push(mg.polygonize(density))

    mg.smooth(0.5)
    mg.color(color { 0.5, 0.4, 0.3, 1.0 })
    model_tree(9, 2.0, 0.7, 15, color { 0.4, 0.6, 0.4, 1.0 })
    trees.push(mg.polygonize(density))

var landscape = nil
// Currently expensive way to do a landscape.
if true:
    mg.smooth(0.1)
    mg.color(color { 0.6, 0.5, 0.4, 1.0 })
    mg.landscape(0.5, 1.0)
    landscape = mg.polygonize(density)

if gun: print "vertcount: gun {gl.mesh_size(gun)}"
if spaceship: print "vertcount: ship {gl.mesh_size(spaceship)}"
for trees: print("vertcount: tree1 {gl.mesh_size(_)}")
if rounded_cube: print "vertcount: rounded_cube {gl.mesh_size(rounded_cube)}"

print seconds_elapsed()

while gl.frame():
    if gl.button("escape") == 1: return

    gl.window_title("meshgen test: {gl.delta_time()}")

    gl.clear(color_darkest_grey)

    let fovscale = 1.5

    if true:
        gl.cursor(false)
        gl.perspective(60.0, 0.1, 1000.0)

        var speed = 10
        if gl.button("left shift") >= 1: speed = 50
        camera.FPS_update("w", "a", "s", "d", speed, 4.0, true)
        camera.FPS_view()
        gl.light(camera.position, float2 { 128.0, 0.1 })
    else:
        gl.ortho3d(float3_0, float3 { gl.window_size().x, gl.window_size().y, 2000 } / 100)
        gl.rotate_x(sincos(45))
        gl.rotate_z(sincos(45))
        gl.light(float3(sincos(gl.time() * 10), 0) * 100 + float3_z * 300, float2 { 128.0, 0.1 })

    gl.blend 0:
        gl.set_shader(aspoints and "phong_particle" or "flat")

        if spaceship:
            gl.translate(float2_1 * 10.0):
                gl.point_scale(1.0 * fovscale)
                gl.render_mesh(spaceship)

        if gun:
            gl.translate(float2_1 * 20.0):
                gl.scale(0.2):
                    gl.point_scale(0.2 * fovscale)
                    gl.render_mesh(gun)

        if landscape:
            gl.translate(float2_y * 30.0):
                gl.scale(10):
                    gl.point_scale(10.0 * fovscale)
                    gl.render_mesh(landscape)

        if rounded_cube:
            gl.translate(float2_y * 10.0):
                gl.scale(2):
                    gl.point_scale(3.0 * fovscale)
                    gl.render_mesh(rounded_cube)

        for(10) j:
            gl.translate(float3_y * j * -5):
                for(trees) t, i:
                    gl.translate(float3_x * i * 10.0):
                        gl.scale(0.5):
                            gl.point_scale(0.5 * fovscale)
                            gl.render_mesh(t)

    gl.set_shader("color")
    gl.debug_grid(int3 { 20, 20, 0 }, float3_1, 0.005)

