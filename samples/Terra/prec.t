local a = &double -> double
assert(a:ispointer())
assert(a.type:isfunction())
assert(a.type.parameters[1]:ispointer())