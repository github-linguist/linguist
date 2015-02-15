| composer fg |
composer := [ :f :g | [ :x | f value: (g value: x) ] ].
fg := composer value: [ :x | x + 1 ]
               value: [ :x | x * x ].

(fg value:3) displayNl.
