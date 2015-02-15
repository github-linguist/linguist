with Pig; with Ada.Text_IO; with Ada.Command_Line;

procedure automatic_Pig is

   use Pig;

   type Robot is new Actor with record
      Bound: Natural := 20;
      Final_Run: Natural := 0;
   end record;
   function Roll_More(A: Robot; Self, Opponent: Player'Class) return Boolean;

   function Roll_More(A: Robot; Self, Opponent: Player'Class) return Boolean is
     ((Self.All_Recent < A.Bound) or
         else (Opponent.Score-100 > A.Final_Run));

   function Arg(Position: Positive; Default: Natural) return Natural is
      package ACL renames Ada.Command_Line;
   begin
      return Natural'Value(ACL.Argument(Position));
   exception
      when Constraint_Error => return Default;
   end Arg;

   T: Robot := (Bound => Arg(2, 35), Final_Run => Arg(3, 0));
   F: Robot := (Bound => Arg(4, 20), Final_Run => Arg(5, 30));

   T_Wins: Boolean;
   Win_Count: array(Boolean) of Natural := (True=> 0, False => 0);
begin
   for I in 1 .. Arg(1, 1000) loop
      Play(T, F, T_Wins);
      Win_Count(T_Wins) := Win_Count(T_Wins) + 1;
   end loop;
   Ada.Text_IO.Put_Line(Natural'Image(Win_Count(True)) &
                          Natural'Image(Win_Count(False)));
end Automatic_Pig;
