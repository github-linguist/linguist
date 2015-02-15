|A B|
A := Set new.
B := Set new.
A addAll: #( 'John' 'Bob' 'Mary' 'Serena' ).
B addAll: #( 'Jim' 'Mary' 'John' 'Bob' ).

( (A - B) + (B - A) ) displayNl.
