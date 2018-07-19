    a←'abcdefghijklmnopqrstuvwxyz'
    A←'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    Panagram←{∧/ ∨⌿ 2 26⍴(a,A) ∊ ⍵}
    Panagram 'This should fail'
0
    Panagram 'The quick brown fox jumps over the lazy dog'
1
