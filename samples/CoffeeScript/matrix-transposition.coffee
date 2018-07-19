transpose = (matrix) ->
    (t[i] for t in matrix) for i in [0...matrix[0].length]
