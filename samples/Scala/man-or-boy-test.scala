def A(in_k: Int, x1: =>Int, x2: =>Int, x3: =>Int, x4: =>Int, x5: =>Int): Int = {
    var k = in_k
    def B: Int = {
        k = k-1
        A(k, B, x1, x2, x3, x4)
    }
    if (k<=0) x4+x5 else B
}
println(A(10, 1, -1, -1, 1, 0))
