require"lpeg"
S, C = lpeg.S, lpeg.C
function ispangram(s)
  return #(C(S(s)^0):match"abcdefghijklmnopqrstuvwxyz") == 26
end

print(ispangram"waltz, bad nymph, for quick jigs vex")
print(ispangram"bobby")
print(ispangram"long sentence")
