import vec
import color
import gl
import texture

// Example of how to do run a custom shader as a post-process on a scene.
// In this case, a simple sobel edge detection filter example adapted from
// https://www.shadertoy.com/view/4t3XDM

fatal(gl.window("custom shader", 1024, 1024))

let mats =
"""
SHADER sobel
    VERTEX
        INPUTS apos:4 atc:2
        UNIFORMS mvp
        gl_Position = mvp * apos;
        itc = atc;
    PIXEL
        INPUTS itc:2
        UNIFORMS tex0 framebuffer_size
        vec3 TL = texture(tex0, itc + vec2(-1,  1)/ framebuffer_size).rgb;
        vec3 TM = texture(tex0, itc + vec2( 0,  1)/ framebuffer_size).rgb;
        vec3 TR = texture(tex0, itc + vec2( 1,  1)/ framebuffer_size).rgb;
        vec3 ML = texture(tex0, itc + vec2(-1,  0)/ framebuffer_size).rgb;
        vec3 MR = texture(tex0, itc + vec2( 1,  0)/ framebuffer_size).rgb;
        vec3 BL = texture(tex0, itc + vec2(-1, -1)/ framebuffer_size).rgb;
        vec3 BM = texture(tex0, itc + vec2( 0, -1)/ framebuffer_size).rgb;
        vec3 BR = texture(tex0, itc + vec2( 1, -1)/ framebuffer_size).rgb;
        vec3 grad_x = -TL + TR - 2.0 * ML + 2.0 * MR - BL + BR;
        vec3 grad_y = TL + 2.0 * TM + TR - BL - 2.0 * BM - BR;
        frag_color.r = length(vec2(grad_x.r, grad_y.r));
        frag_color.g = length(vec2(grad_x.g, grad_y.g));
        frag_color.b = length(vec2(grad_x.b, grad_y.b));
"""

fatal(gl.load_materials(mats, true))

var tex = nil

while(gl.frame()):
    if gl.button("escape") == 1: return
    // Render the body to a texture, then the post-process of it to
    // the screen.
    tex = post_process(tex, gl.window_size(), "sobel", false,
                       texture_format_nomipmap):
        rnd_seed(0)
        gl.clear(color_black)
        for(256) i:
            gl.translate rnd_float2() * (sincos(gl.time() * 10.0 + i) * 0.5 + 0.5) * float(gl.window_size()):
                gl.circle(50.0, 7)
