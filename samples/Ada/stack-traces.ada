with Ada.Text_IO;              use Ada.Text_IO;
with GNAT.Traceback;           use GNAT.Traceback;
with GNAT.Traceback.Symbolic;  use GNAT.Traceback.Symbolic;

procedure Test_Stack_Trace is
   procedure Call_Stack is
      Trace  : Tracebacks_Array (1..1_000);
      Length : Natural;
   begin
      Call_Chain (Trace, Length);
      Put_Line (Symbolic_Traceback (Trace (1..Length)));
   end Call_Stack;

   procedure Inner (K : Integer) is
   begin
      Call_Stack;
   end Inner;

   procedure Middle (X, Y : Integer) is
   begin
      Inner (X * Y);
   end Middle;

   procedure Outer (A, B, C : Integer) is
   begin
     Middle (A + B, B + C);
   end Outer;

begin
  Outer (2,3,5);
end Test_Stack_Trace;
