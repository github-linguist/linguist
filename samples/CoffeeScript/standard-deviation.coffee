class StandardDeviation
    constructor: ->
        @sum = 0
        @sumOfSquares = 0
        @values = 0
        @deviation = 0

    include: ( n ) ->
        @values += 1
        @sum += n
        @sumOfSquares += n * n
        mean = @sum / @values
        mean *= mean
        @deviation = Math.sqrt @sumOfSquares / @values - mean

dev = new StandardDeviation
values = [ 2, 4, 4, 4, 5, 5, 7, 9 ]
tmp = []

for value in values
    tmp.push value
    dev.include value
    console.log """
        Values: #{ tmp }
        Standard deviation: #{ dev.deviation }

    """
