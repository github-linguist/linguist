Y = Function[f, #@# &@Function[x, f[x[x]@# &]]];
factorial = Y@Function[f, If[# < 1, 1, # f[# - 1]] &];
fibonacci = Y@Function[f, If[# < 2, #, f[# - 1] + f[# - 2]] &];
