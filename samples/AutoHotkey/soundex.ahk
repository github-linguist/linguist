getCode(c){
        If c in B,F,P,V
            return 1
        If c in C,G,J,K,Q,S,X,Z
            return 2
        If c in D,T
            return 3
        If c = L
            return 4
        If c in M,N
            return 5
        If c = R
            return 6
}

soundex(s){
    code := SubStr(s, 1, 1)
   ,previous := 7
   ,i := 1
    While ++i <= StrLen(s){
        current := getCode(SubStr(s, i, 1))
        If StrLen(current) > 0 And current <> previous
            code := code . current
        previous := current
    }
    soundex := SubStr(code, 1, 4)
    If StrLen(code) < 4
        soundex .= String(4 - StrLen(code), "0")
    return soundex
}

String(a, n){
   Loop n
      o .= a
   return a
}

MsgBox % Soundex("Soundex") "`n" Soundex("Sowndeks") "`n" Soundex("Ashcroft") "`n" Soundex("Ashkrofd")
