var x := 0
__loop(fn {
    x += 1
    println(x)
    x % 6 != 0   # this is the return value of the function
})
