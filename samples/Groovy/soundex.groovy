def soundex(s) {
    def code = ""
    def lookup = [
       B : 1, F : 1, P : 1, V : 1,
       C : 2, G : 2, J : 2, K : 2, Q : 2, S : 2, X : 2, Z : 2,
       D : 3, T : 3,
       L : 4,
       M : 5, N : 5,
       R : 6
    ]
    s[1..-1].toUpperCase().inject(7) { lastCode, letter ->
        def letterCode = lookup[letter]
        if(letterCode && letterCode != lastCode) {
            code += letterCode
        }
    }
    return "${s[0]}${code}0000"[0..3]
}

println(soundex("Soundex"))
println(soundex("Sownteks"))
println(soundex("Example"))
println(soundex("Ekzampul"))
