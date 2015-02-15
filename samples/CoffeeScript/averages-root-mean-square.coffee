    root_mean_square = (ary) ->
        sum_of_squares = ary.reduce ((s,x) -> s + x*x), 0
        return Math.sqrt(sum_of_squares / ary.length)

    alert root_mean_square([1..10])
