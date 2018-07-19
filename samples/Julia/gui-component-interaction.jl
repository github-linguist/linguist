using Tk
w = Toplevel("Component Interaction Example")
fr = Frame(w)
pack(fr, {:expand=>true, :fill => "both"})
## The task: For a minimal "application", write a program that
## presents a form with three components to the user: A numeric input
## field ("Value") and two buttons ("increment" and "random").

value = Entry(fr, "")
increment = Button(fr, "Increment")
random = Button(fr, "Random")

formlayout(value, "Value:")
formlayout(increment, " ")
formlayout(random, " ")

set_value(value, "0") ## The field is initialized to zero.

tk_bind(increment, "command") do path  ## increment its value with the "increment" button.
  val = get_value(value) | float
  set_value(value, string(val + 1))
end


function validate_command(path, P)
    try
        if length(P) > 0 parsefloat(P) end
        tcl("expr", "TRUE")
    catch e
        tcl("expr", "FALSE")
    end
end
function invalid_command(path, W)
    println("Invalid value")
    tcl(W, "delete", "@0", "end")
end

tk_configure(value, {:validate=>"key", :validatecommand=>validate_command, :invalidcommand=>invalid_command })

## Pressing the "random" button presents a confirmation dialog, and resets the field's value to a random value if the answer is "Yes".
tk_bind(random, "command") do path
   out = Messagebox(w, "Randomize input", "Select a new random number?")
   if out == "ok"
       new_value = floor(100*rand(1))[1]
       set_value(value, string(new_value))
   end
end
