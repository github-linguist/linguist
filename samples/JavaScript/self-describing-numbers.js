function is_self_describing(n) {
    var digits = Number(n).toString().split("").map(function(elem) {return Number(elem)});
    var len = digits.length;
    var count = digits.map(function(x){return 0});

    digits.forEach(function(digit, idx, ary) {
        if (digit >= count.length)
            return false
        count[digit] ++;
    });

    return digits.equals(count);
}

Array.prototype.equals = function(other) {
    if (this === other)
        return true;  // same object
    if (this.length != other.length)
        return false;
    for (idx in this)
        if (this[idx] !== other[idx])
            return false;
    return true;
}

for (var i=1; i<=3300000; i++)
    if (is_self_describing(i))
        print(i);
