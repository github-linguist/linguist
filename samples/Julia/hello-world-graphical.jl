using Tk

window = Toplevel("Hello World", 200, 100, false)
pack_stop_propagate(window)

fr = Frame(window)
pack(fr, expand=true, fill="both")

txt = Label(fr, "Hello World")
pack(txt, expand=true)

set_visible(window, true)

# sleep(7)
