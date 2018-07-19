\ Short-circuit evaluation definitions from Wil Baden, with minor name changes
:  ENDIF  postpone THEN ; immediate

:   COND  0 ; immediate
: ENDIFS  BEGIN DUP WHILE postpone ENDIF REPEAT DROP ; immediate
: ORELSE  s" ?DUP 0= IF"  evaluate ; immediate
:  ANDIF  s" DUP IF DROP" evaluate ; immediate

: .bool IF ." true  " ELSE ." false " THEN ;
: A  ." A=" DUP .bool ;
: B  ." B=" DUP .bool ;

: test
  CR
  1 -1 DO 1 -1 DO
    COND I A ANDIF  J B ENDIFS ." ANDIF="  .bool CR
    COND I A ORELSE J B ENDIFS ." ORELSE=" .bool CR
  LOOP LOOP ;

\ An alternative based on explicitly short-circuiting conditionals, Dave Keenan
: END-PRIOR-IF  1 CS-ROLL postpone ENDIF ; immediate

: test
  CR
  1 -1 DO 1 -1 DO
    I A    IF J B IF 1 ELSE END-PRIOR-IF 0 ENDIF ." ANDIF="  .bool CR
    I A 0= IF J B IF END-PRIOR-IF 1 ELSE 0 ENDIF ." ORELSE=" .bool CR
  LOOP LOOP ;
