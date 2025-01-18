// This example renders a cube with a different colorèd number on each face.
// Moving the mouse or WASD rotates the viewport around the cube like turning a
// globe in front of the camera.
//
// The example illustrates creating a simple mesh and rendering a texture on
// the mesh in various orientations.
// The example showcases the render_to_texture utility, which allows us to
// render a 2D drawing on the surfaces of a 3D object, using a frame buffer.
// The example also demostrates using gl.new_mesh, which allows us to orient
// the texture on each face of the cube, since the simpler gl.new_poly is not
// quite sufficient for this purpose.
//
// To generate the mesh for each face, cube_face_meshes uses the three least
// significant bits of ASCII characters to represent whether the edge is on the
// near or far side of the cube along each axis.
//
//  -----ZYX CHR corner
//  00110000 '0' origin
//  00110001 '1' x
//  00110010 '2' y
//  00110011 '3' xy
//  00110100 '4' z
//  00110101 '5' xz
//  00110110 '6' yz
//  00110111 '7' zyz
//
// Each face of the cube contains four of the cube's vertices.
// The normal vector for the face of the cube must face outward to be opaque to
// an outside observer, so the vertices are listed counter-clockwise.
// In the following illustration, the interior faces are inverted, so the
// vertices appear in clockwise order from our perspective.
//
// The first index must be the top-right corner of the texture.
// The textures are arranged such that the textures are upright around the
// equator and the poles are connected top to bottom with their nearest
// neighbor.
// Rotating the vertex strings rotates the corresponding texture orientation.
//
// The faces are numbered according to the conventions of right-handed dice.
// All faces in opposition have the same sum.
// Numbers read counter-clockwise about the origin and its opposite vertex.
//
//                                 inverted clockwise
//  Z       4---5             .---.   .---.
//   \      |\  |\    1540->2 |\ 2 \  |   |\  6<-5464
//    0--X  | 0---1           | .---. | 6 | .
//    |     | | | |   0462->3 |3|   | |   |4| 4<-5137
//    Y     6-|-7 |           ' | 1 | '---' |
//           \|  \|   3102->1  \|   |  \ 5 \| 5<-7326
//            2---3             '---'   '---'
//                    counter-clockwise
//
// Kris Kowal <kris@cixar.com>

import vec
import color
import gl
import texture
import camera

fatal(gl.window("Lobster Cube", 515, 515))
check(gl.set_font_name("data/fonts/Droid_Sans/DroidSans.ttf"), "can\'t load font")

// cube_face_meshes contains the meshes for the six faces of a cube,
// facing outward.
let cube_face_vertices = [
    "3102", // 1
    "1540", // 2
    "0462", // 3
    "5137", // 4
    "7326", // 5
    "5764", // 6
]

// These are in order from top-right, counter-clockwise.
// I can offer no explanation why the origin is not the top- or bottom-left.
let square = [float2_0, float2_x, float2_1, float2_y]

let cube_face_meshes = map(cube_face_vertices) v:
    let positions = map(v) c: vec3_v(map(3) i: float(c & (1 << (2 - i)) != 0))
    let indices = [0, 1, 2, 2, 3, 0]
    gl.new_mesh(
        "PT",
        positions, // "P"
        [], // colors,
        [], // normals,
        square, // "T" texcoords,
        [], // textcoords2,
        indices
    )

// Use the frame buffer to render a unique texture for each face of the cube,
// with its number.
// We use white on grey since we can use these as color multipliers where we
// render the mesh.
let detail = 256
let cube_face_textures = map(6) i:
    render_to_texture(nil, int2_1 * detail, false, nil, texture_format_nomipmap):
        gl.ortho(true, false)
        let label = "{i+1}"
        gl.set_font_size(detail/2)
        let size = gl.text_size(label)
        gl.translate(float2_1 * float(detail) / 2.0 - float(size) / 2.0)
        gl.clear(color_grey)
        gl.color(color_white)
        gl.text(label)

// Colors are arranged such that CMY are about the origin and RGB on the polar
// opposites.
// Colors on opposite faces are also opposite hues.
let face_colors = [
    color_purple,     // M
    color_olive,      // Y
    color_teal,       // C
    color_dark_red,   // R
    color_dark_blue,  // G
    color_dark_green, // B
]

// Rotate the camera to place the origin vertex of the cube in the center of
// the view.
let camera = Camera { float3_0, -45.0, 45.0 }

// This demo is able to use camera.FPS_view but uses a different control model
// to move the camera about the origin at a fixed “elevation”.
def camera_GPS_update(upkey, leftkey, downkey, rightkey, elevation:float, mousesens:float, keyspeed:float):
    let long = (gl.button(upkey) >= 1) - (gl.button(downkey) >= 1)
    let lat = (gl.button(rightkey) >= 1) - (gl.button(leftkey) >= 1)
    camera.pitch -= gl.mouse_delta(0).y / mousesens + long * keyspeed
    camera.yaw -= gl.mouse_delta(0).x / mousesens - lat * keyspeed
    camera.pitch = min(85.0, max(-85.0, camera.pitch))
    camera.position = vecfromyawpitch(camera.yaw, camera.pitch, -elevation, 0.0)

while gl.frame() and not gl.button("escape"):
    gl.clear(color_dark_grey)
    gl.cursor(false)
    gl.perspective(60.0, 0.1, 1000.0)

    camera_GPS_update("w", "a", "s", "d", 2.0, 4.0, 4.0)
    camera.FPS_view()

    gl.translate(float3_1 / -2.0)
    gl.set_shader("textured")
    for(6) i:
        // The texture colors are multiplied by the color in context.
        // Since the texture on our mesh is white on black, we can change the
        // white to a unique color for each face of the world.
        gl.color(face_colors[i])
        gl.set_primitive_texture(0, cube_face_textures[i])
        gl.render_mesh(cube_face_meshes[i])
