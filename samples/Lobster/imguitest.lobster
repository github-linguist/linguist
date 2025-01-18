// simple demo showing integration of https://github.com/ocornut/imgui

import vec
import color
import gl
import imgui
import gl

fatal(gl.window("imgui demo", 1280, 1024))

im.init(false, im.config_docking_enable, 3.0)
assert im.add_font("data/fonts/Droid_Sans/DroidSans.ttf", 20.0)

// The data we're editing:
class Thingy:
    on = true
    i = 1
    ratio = 0.5
    name = "Fred"
    col = color_red
    pos = float2_1
    fruits = ["apple", "banana", "pear"]
    others:[Thingy]
    windowflag = im.window_always_autoresize

var d = Thingy { map(3): Thingy { [] } }
var demo = false
let foo = "bar"  // Will show up in debug view.

while gl.frame():
    gl.clear(color_grey)

    // <- Render game here.

    im.frame():
        // Maximum convenience: turn any data structure into editable UI!
        im.window("Automatically created from a class", d.windowflag):
            d = im.edit_anything(d)

        // Manually created widgets, so you can choose how to represent and layout data:
        im.window("Created by code", im.window_no_collapse | im.window_always_autoresize):
            im.tab_bar("tab_bar"):
                im.tab("Widgets"):
                    im.text("A text!")
                    d.on = im.checkbox("Check it out!", d.on)
                    im.tooltip("This is worth checking out, really!")
                    d.ratio = im.sliderfloat("Float", d.ratio, 0.0, 1.0)
                    d.i = im.sliderint("Int", d.i, 0, 10)
                    d.name = im.input_text("Name", d.name)
                    d.i = im.radio(d.fruits, d.i, true)
                    d.i = im.combo("Combo", d.fruits, d.i)
                    d.i = im.listbox("ListBox", d.fruits, d.i, d.fruits.length)
                    d.col = im.coloredit("Color", d.col)
                    for(2) g: im.graph("Sin", map(20): sin(_ * 20.0) + 1.0, g)
                    if im.button("popup!"):
                        im.popup_open("pop1")
                    im.popup("pop1"):
                        im.selectable("select a")
                        im.selectable("select b")
                im.tab("Debug"):
                    if not demo and im.button("show demo window"): demo = true
                    if im.button("Cause breakpoint() hit"):
                        // The debugger should have popped up!
                        // The program is frozen until you hit continue.
                        // Try putting these breakpoints anywhere in your own code.
                        breakpoint()

        // Show the built-in imgui demo window, for testing.
        if demo: demo = im.window_demo()

        // Realtime debugging! Show all lobster variables as editable UI.
        // Also show some engine stats, like e.g. a framerate graph.
        // These are shown in realtime, if you want to pause and see local
        // variables in functions, try breakpoint() above
        im.window("Realtime Debugger", im.window_always_autoresize):
            im.show_engine_stats()
            im.treenode("Memory Usage"):
                im.text(get_memory_usage(20))
            im.show_vars()
