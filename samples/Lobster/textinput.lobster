// Shows how you could input text strings.

import std
import vec
import color
import gl

gl.window("text input", 1200, 600)
check(gl.set_font_name("data/fonts/Inconsolata/Inconsolata-Regular.ttf"), "font!")

gl.start_text_input(int2_0, int2_1 * gl.get_font_size())

let log = []

while gl.frame():
    gl.clear(color_black)
    gl.set_font_size(30)
    // This returns other values to render IME correctly, which we'll skip here.
    let text = gl.text_input_state()
    gl.text(text + "_")
    if gl.button("return") == 1:
        // Here we normally end the input.
        log.insert(0, text)
        gl.end_text_input()
        // For now, go right back to inputting a new string:
        gl.start_text_input(int2_0, int2_1 * gl.get_font_size())
    elif gl.key_repeat("backspace") == 1:
        // Inefficient but simple way to drop last character.
        let uv = string_to_unicode(text)
        if uv.length:
            uv.pop()
            gl.set_text_input(unicode_to_string(uv))
    // Render all strings input so far:
    for(log) l, i:
        gl.translate(float2 { 0.0, gl.get_font_size() * (i + 2.0) }):
            gl.text(l)
