Array extend [
    difference [
        ^self allButFirst with: self allButLast collect: [ :a :b | a - b ]
    ]

    nthOrderDifference: n [
        ^(1 to: n) inject: self into: [ :old :unused | old difference ]
    ]
]

s := #(90 47 58 29 22 32 55 5 55 73)
1 to: s size - 1 do: [ :i |
    (s nthOrderDifference: i) printNl ]
