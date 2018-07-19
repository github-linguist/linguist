coclass 'AA'
NB. associative array

create=: verb define
 empty KEYS=: DATA =: ''
)

destroy=: codestroy

put=: dyad define  NB. DATUM put KEY
 DATA=: DATA , < x
 KEYS=: KEYS , < y
 EMPTY
)

get=: verb define NB. get KEY
 (KEYS (i. <) y) {:: DATA
)

cocurrent'base'

get =: dyad define  NB. OBJECT get KEY
 try.
  get__x y
 catch.
  smoutput '?' ,~ ": y
  RV =. 1!:1<1
  RV put__x y
  RV
 end.
)

STORY =: 0 :0
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.
)

madlib =: verb define NB. madlib STORY
 I =. y i. '<'
 if. I = # y do. y return. end.         NB. no substitutions
 HEAD =. I {. y
 TAIL =. I }. y
 A =. }. (<;.1~ '<'&=) '<' , TAIL  NB. the story is parsed by '<'
 B =. (({. ; }.)~ >:@:i.&'>')&> A
 NB.+-----------+------------------------------+
 NB.|<name>     | went for a walk in the park. |
 NB.+-----------+------------------------------+
 NB.|<he or she>| found a                      |
 NB.+-----------+------------------------------+
 NB.|<noun>     |.                             |
 NB.+-----------+------------------------------+
 NB.|<name>     | decided to take it home.     |
 NB.+-----------+------------------------------+
 AA =. conew'AA'
 create__AA''
 SUBSTITUTIONS =. AA&get&.> _ 1 {. B
 codestroy__AA''
 HEAD , ; SUBSTITUTIONS ,. 0 1 }. B
)
