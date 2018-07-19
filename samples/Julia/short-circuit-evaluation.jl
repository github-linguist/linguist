a(x) = (println("\t# Called a($x)"); return x)
b(x) = (println("\t# Called b($x)"); return x)

for i in [true,false], j in [true, false]
    println("\nCalculating: x = a($i) && b($j)"); x = a(i) && b(j)
    println("\tResult: x = $x")
    println("\nCalculating: y = a($i) || b($j)"); y = a(i) || b(j)
    println("\tResult: y = $y")
end
