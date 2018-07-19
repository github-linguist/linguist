using Tk
w = Toplevel("GUI enabling/disabling")
fr = Frame(w)
pack(fr, {:expand=>true, :fill => "both"})

value = Entry(fr)
increment = Button(fr, "+")
decrement = Button(fr, "-")

formlayout(value, "Value:")
formlayout(increment, " ")
formlayout(decrement, " ")

## value stores a string
set_value(value, "0") ## The field is initialized to zero.
get(value::Tk_Entry) = try parseint(get_value(value)) catch e nothing end

function update()
    cur_value = get(value)
    set_enabled(value,     isa(cur_value, Integer) && cur_value == 0)
    set_enabled(increment, isa(cur_value, Integer) && cur_value < 10)
    set_enabled(decrement, isa(cur_value, Integer) && cur_value > 0)
end

crement = function(step)
  set_enabled(value, true)
  set_value(value, string(get(value) + step))
  update()
end
tk_bind(increment, "command", path -> crement(1))
tk_bind(decrement, "command", path -> crement(-1))
update()

function validate_command(path, P)
    try
        if length(P) > 0 parseint(P); update() end
        tcl("expr", "TRUE")
    catch e
        tcl("expr", "FALSE")
    end
end
function invalid_command(path, W)
    println("Invalid value")
    tcl(W, "delete", "@0", "end")
end

tk_configure(value, {:validate=>"focusout", :validatecommand=>validate_command, :invalidcommand=>invalid_command })
