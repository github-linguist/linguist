begin
   integer i;
   procedure do_it (n, L); value n; integer n; label L;
   begin
      integer array A [1 : n];
      integer i;
      for i := 1 step 1 until n do A [i] := i * i;
      if n < 10 then goto L else do_it (n - 1, L);
   end;
   outstring (1, "Example 5: arrays with non constant bounds\n");
   for i := 1 step 1 until 2000 do
   begin
      do_it (2000, L_end);
   L_end:;
   end;
end;


