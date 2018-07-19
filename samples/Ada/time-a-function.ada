with Ada.Calendar; use Ada.Calendar;
with Ada.Text_Io; use Ada.Text_Io;

procedure Query_Performance is
   type Proc_Access is access procedure(X : in out Integer);
   function Time_It(Action : Proc_Access; Arg : Integer) return Duration is
      Start_Time : Time := Clock;
      Finis_Time : Time;
      Func_Arg : Integer := Arg;
   begin
      Action(Func_Arg);
      Finis_Time := Clock;
      return Finis_Time - Start_Time;
   end Time_It;
   procedure Identity(X : in out Integer) is
   begin
      X := X;
   end Identity;
   procedure Sum (Num : in out Integer) is
   begin
      for I in 1..1000 loop
         Num := Num + I;
      end loop;
   end Sum;
   Id_Access : Proc_Access := Identity'access;
   Sum_Access : Proc_Access := Sum'access;

begin
   Put_Line("Identity(4) takes" & Duration'Image(Time_It(Id_Access, 4)) & " seconds.");
   Put_Line("Sum(4) takes:" & Duration'Image(Time_It(Sum_Access, 4)) & " seconds.");
end Query_Performance;
