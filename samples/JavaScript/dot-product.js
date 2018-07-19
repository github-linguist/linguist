function dot_product(ary1, ary2) {
    if (ary1.length != ary2.length)
        throw "can't find dot product: arrays have different lengths";
    var dotprod = 0;
    for (var i = 0; i < ary1.length; i++)
        dotprod += ary1[i] * ary2[i];
    return dotprod;
}

print(dot_product([1,3,-5],[4,-2,-1])); // ==> 3
print(dot_product([1,3,-5],[4,-2,-1,0])); // ==> exception
