# At top level, global variables are declared when they are assigned, so one only writes
global_var := 1;

# In a function, local variables are declared like this
func := function(n)
    local a;
    a := n*n;
    return n + a;
end;

# One can test whether a variable is assigned
IsBound(global_var);
# true;

# And destroy a variable
Unbind(global_var);

# This works with list elements too
u := [11, 12, , 14];
IsBound(u[4]);
# true
IsBound(u[3]);
# false
Unbind(u[4]);
