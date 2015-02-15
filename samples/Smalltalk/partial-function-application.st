| f1 f2 fs fsf1 fsf2 partial |

partial := [ :afs :af | [ :s | afs value: af value: s ] ].

fs := [ :f :s | s collect: [ :x | f value: x ]].
f1 := [ :x | x * 2 ].
f2:= [ :x | x * x ].
fsf1 := partial value: fs value: f1.
fsf2 := partial value: fs value: f2.

fsf1 value: (0 to: 3).
" #(0 2 4 6)"
fsf2 value: (0 to: 3).
" #(0 1 4 9)"

fsf1 value: #(2 4 6 8).
" #(4 8 12 16)"
fsf2 value: #(2 4 6 8).
" #(4 16 36 64)"
