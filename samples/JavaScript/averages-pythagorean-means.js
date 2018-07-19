function arithmetic_mean(ary) {
    var sum = ary.reduce(function(s,x) {return (s+x)}, 0);
    return (sum / ary.length);
}

function geometic_mean(ary) {
    var product = ary.reduce(function(s,x) {return (s*x)}, 1);
    return Math.pow(product, 1/ary.length);
}

function harmonic_mean(ary) {
    var sum_of_inv = ary.reduce(function(s,x) {return (s + 1/x)}, 0);
    return (ary.length / sum_of_inv);
}

var ary = [1,2,3,4,5,6,7,8,9,10];
var A = arithmetic_mean(ary);
var G = geometic_mean(ary);
var H = harmonic_mean(ary);

print("is A >= G >= H ? " + (A >= G && G >= H ? "yes" : "no"));
