with Ada.Text_IO;

procedure Hofstadter_Q_Sequence is

   type Callback is access procedure(N: Positive);

   procedure Q(First, Last: Positive; Q_Proc: Callback) is
   -- calls Q_Proc(Q(First)); Q_Proc(Q(First+1)); ... Q_Proc(Q(Last));
   -- precondition: Last > 2

      Q_Store: array(1 .. Last) of Natural := (1 => 1, 2 => 1, others => 0);
      -- "global" array to store the Q(I)
      -- if Q_Store(I)=0, we compute Q(I) and update Q_Store(I)
      -- else we already know Q(I) = Q_Store(I)

      function Q(N: Positive) return Positive is
      begin
         if Q_Store(N) = 0 then
            Q_Store(N) := Q(N - Q(N-1)) + Q(N-Q(N-2));
         end if;
         return Q_Store(N);
      end Q;

   begin
      for I in First .. Last loop
         Q_Proc(Q(I));
      end loop;
   end Q;

   procedure Print(P: Positive) is
   begin
      Ada.Text_IO.Put(Positive'Image(P));
   end Print;

   Decrease_Counter: Natural := 0;
   Previous_Value: Positive := 1;

   procedure Decrease_Count(P: Positive) is
   begin
      if P < Previous_Value then
         Decrease_Counter := Decrease_Counter + 1;
      end if;
      Previous_Value := P;
   end Decrease_Count;

begin
   Q(1, 10, Print'Access);
   -- the first ten terms of the sequence are: 1, 1, 2, 3, 3, 4, 5, 5, 6, and 6
   Ada.Text_IO.New_Line;

   Q(1000, 1000,  Print'Access);
   -- the 1000'th term is: 502
   Ada.Text_IO.New_Line;

   Q(2, 100_000, Decrease_Count'Access);
   Ada.Text_IO.Put_Line(Integer'Image(Decrease_Counter));
   -- how many times a member of the sequence is less than its preceding term
   -- for terms up to and including the 100,000'th term
end Hofstadter_Q_Sequence;
