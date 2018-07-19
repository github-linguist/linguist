def makeNounExpr := <elang:evm.makeNounExpr>

def dynVarName(name) {
    def variable := makeNounExpr(null, name, null)

    return e`{

        def a := 1
        def b := 2
        def c := 3

        {
            def $variable := "BOO!"
            [a, b, c]
        }

    }`.eval(safeScope)
}

? dynVarName("foo")
# value: [1, 2, 3]

? dynVarName("b")
# value: [1, "BOO!", 3]

? dynVarName("c")
# value: [1, 2, "BOO!"]
