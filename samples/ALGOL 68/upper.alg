COMMENT
  An example in UPPER stropping, as in the Revised Report and in
  Algol 68 Genie: reserved words and modes in capitals, identifiers in
  lower case, also with blanks (max int, stand out).
COMMENT
PR include "util.a68" PR
BEGIN
   # modes, operators and their priorities #
   MODE NODE = STRUCT (INT value, REF NODE next);
   MODE NUMBER = UNION (INT, REAL, LONG COMPL);
   PRIO MAX = 9;
   OP MAX = (INT a, b) INT: (a > b | a | b);
   OP +:= = (REF NODE n, INT k) REF NODE: n := HEAP NODE := (k, n);

   CO procedures and routine texts CO
   PROC sum of list = (REF NODE list) INT:
   BEGIN
      INT s := 0;
      REF NODE p := list;
      WHILE p ISNT NIL DO s +:= value OF p; p := next OF p OD;
      s
   END;
   PROC (INT) INT square = (INT n) INT: n * n;

   NOTE nested NOTE comments ETON (GNU) ETON
   { a brief comment, as in ga68 }
   REF NODE list := NIL;
   FOR i FROM 1 BY 2 TO 9 DO list +:= i OD;
   print (("sum: ", whole (sum of list (list), 0), new line));
   print ((3 MAX 7, ABS -4, ODD 5, max int, long max real, pi, new line));

   # denotations #
   REAL x = 1.5e-3, y = .25, z = 1 000.5;
   BITS mask = 16r ff, b = 2r1010;
   LONG COMPL c = 4.0 I 5.0;
   CHAR q = """";
   STRING s = "a ""quoted"" word, and #, CO, { } in a string";
   [1:3] INT row := (1, 2, 3);
   FLEX [1:0] CHAR text;

   # clauses #
   NUMBER n = 42;
   CASE n IN (INT i): print (i), (REAL r): print (r) OUT print ("?") ESAC;
   IF x < y ANDTH y < z THEN print ("in order") ELIF x = y THEN SKIP ELSE GOTO done FI;
   INT k = (row[1] = 1 | 10 | 20);

   # formatted transput (Revised Report) #
   FORMAT f = $"value: "g(0)l$;
   printf ((f, k));
   printf (($3d, 2x, +d.2d l$, k, x));

   # Algol 68 Genie #
   FOR i TO 3 WHILE i < 3 DO print (i) UNTIL i = 2 OD;
   FOR i FROM 3 DOWNTO 1 DO print (i) OD;
   IF k > 5 ANDF k < 30 THEN print (char in string ("o", LOC INT, "word")) FI;
   print ((utc time, garbage collections, new line));
done:
   SKIP
END
