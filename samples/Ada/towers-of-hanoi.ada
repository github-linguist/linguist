with Ada.Text_Io; use Ada.Text_Io;

procedure Towers is
   type Pegs is (Left, Center, Right);
   procedure Hanoi (Ndisks : Natural; Start_Peg : Pegs := Left; End_Peg : Pegs := Right; Via_Peg : Pegs := Center) is
   begin
      if Ndisks > 0 then
         Hanoi(Ndisks - 1, Start_Peg, Via_Peg, End_Peg);
         Put_Line("Move disk" & Natural'Image(Ndisks) & " from " & Pegs'Image(Start_Peg) & " to " & Pegs'Image(End_Peg));
         Hanoi(Ndisks - 1, Via_Peg, End_Peg, Start_Peg);
      end if;
   end Hanoi;
begin
   Hanoi(4);
end Towers;
