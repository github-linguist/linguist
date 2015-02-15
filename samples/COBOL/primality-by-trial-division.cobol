       Identification Division.
       Program-Id. Primality-By-Subdiv.

       Data Division.
       Working-Storage Section.
       78  True-Val  Value 0.
       78  False-Val Value 1.

       Local-Storage Section.
       01  lim Pic 9(10).
       01  i   Pic 9(10).

       Linkage Section.
       01  num    Pic 9(10).
       01  result Pic 9.

       Procedure Division Using num result.
       Main.
           If num <= 1
               Move False-Val To result
               Goback
           Else If num = 2
               Move True-Val To result
               Goback
           End-If

           Compute lim = Function Sqrt(num) + 1
           Perform Varying i From 3 By 1 Until lim < i
               If Function Mod(num, i) = 0
                   Move False-Val To result
                   Goback
               End-If
           End-Perform

           Move True-Val To Result

           Goback
           .
