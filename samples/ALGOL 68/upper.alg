COMMENT An example in UPPER stropping, as in the Revised Report and Algol 68 Genie;
BEGIN
   MODE NODE = STRUCT (INT value, REF NODE next);
   MODE NUMBER = UNION (INT, REAL, LONG COMPL);
   PRIO MAX = 9;
   OP MAX = (INT a, b) INT: (a > b | a | b);
   OP +:= = (REF NODE n, INT k) REF NODE: n := HEAP NODE := (k, n);

   PROC sum of list = (REF NODE list) INT:
   BEGIN
      INT s := 0;
      REF NODE p := list;
      WHILE p ISNT NIL DO s +:= value OF p; p := next OF p OD;
      s
   END;

   REF NODE list := NIL;
   FOR i FROM 1 BY 2 TO 9 DO list +:= i OD;
   print (("sum: ", whole (sum of list (list), 0), new line));
END
