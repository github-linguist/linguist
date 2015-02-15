function root_mean_square(ary) {
    var sum_of_squares = ary.reduce(function(s,x) {return (s + x*x)}, 0);
    return Math.sqrt(sum_of_squares / ary.length);
}

print( root_mean_square([1,2,3,4,5,6,7,8,9,10]) ); // ==> 6.2048368229954285
