import gui

fatal(gl.window("gui test", 960, 640))
check(gl.set_font_name("data/fonts/US101/US101.ttf"), "can\'t load font!")
//check(gl.set_font_name("C:\\Windows\\Fonts\\arialuni.ttf"), "can\'t load font!")

let ghosttex = gl.load_texture("data/textures/lobster.jpg")
assert ghosttex

var clicked = false
var focused = false

while gl.frame():
    if gl.button("escape") == 1:
        return

    let fontsize = gl.window_size().y / 10
    gl.set_font_size(fontsize, fontsize / 40.0)

    gl.clear(color_grey)
    gl.color(color_white)

    gui.setclickcolors(color_dark_green, color_green)

    gui.start(gl.window_size(), 1, 1, true):
        gui.hblock(1, 0):
            //gui.fontscale(0.3):
                //   gui.text(unicode_to_string(map(500): _))

            gui.text("A")
            gui.fontscale(2):
                gui.text("B")
            gui.vblock(2, 0):
                gui.text("1")
                gui.hblock(1, 0.1):
                    gui.text("2")
                    gui.text("2")
                    gui.text("2")
                gui.divider(2)
                gui.text("3")
                gui.text("4")
            gui.space(0.2)
            gui.divider(4)
            gui.space(0.2)
            gui.text("C")
            gui.button():
                gui.border(int2_1 * 2):
                    gui.fontscale(2):
                        gui.text("clickme")
            fn:
                clicked = not clicked
            fn:
                focused = not focused
            if clicked:
                gui.color(color_red):
                    //gui.text(unicode_to_string([0x30E6, 0x30FC, 0x30B6, 0x30FC, 0x5225, 0x30B5, 0x30A4, 0x30C8]))
                    gui.text("ON")
            gui.text(focused and "C" or "E")
            gui.background(color_blue, false):
                gui.margin(int2_1 * 4):
                    gui.text("D")
                    if(clicked):
                        gui.blend(2):
                            gui.image(float2_1, ghosttex)
            gui.text("E")
            gui.text("E")