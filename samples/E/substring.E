def string := "aardvarks"
def n := 4
def m := 4
println(string(n, n + m))
println(string(n))
println(string(0, string.size() - 1))
println({string(def i := string.indexOf1('d'), i + m)})
println({string(def i := string.startOf("ard"), i + m)})
