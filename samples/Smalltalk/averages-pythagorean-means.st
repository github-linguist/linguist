Collection extend
[
    arithmeticMean
    [
	^ (self fold: [:a :b| a + b ]) / (self size)
    ]

    geometricMean
    [
	^ (self fold: [:a :b| a * b]) raisedTo: (self size reciprocal)
    ]

    harmonicMean
    [
	^ (self size) / ((self collect: [:x|x reciprocal]) fold: [:a :b| a + b ] )
    ]
]

|a|
a := #(1 2 3 4 5 6 7 8 9 10).

a arithmeticMean asFloat displayNl.
a geometricMean asFloat displayNl.
a harmonicMean asFloat displayNl.

((a arithmeticMean) >= (a geometricMean)) displayNl.
((a geometricMean) >= (a harmonicMean)) displayNl.
