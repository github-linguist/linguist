function A(in_k: int, x1(): int, x2(): int, x3(): int, x4(): int, x5(): int): int  {
    var k = in_k
    var B(): int  // B is a function variable
    B = \ -> {
        k = k-1;
        return A(k, B, x1, x2, x3, x4)
    }
    return k<=0 ? x4()+x5() : B()
}
print(A(10, \ -> 1, \ -> -1, \ -> -1, \ -> 1, \ -> 0))
