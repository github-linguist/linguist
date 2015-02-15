with Ada.Text_IO, Ada.Numerics.Float_Random;

procedure Concurrent_Hello is
   type Messages is (Enjoy, Rosetta, Code);
   task type Writer (Message : Messages);
   task body Writer is
      Seed : Ada.Numerics.Float_Random.Generator;
   begin
      Ada.Numerics.Float_Random.Reset (Seed); -- time-dependent, see ARM A.5.2
      delay Duration (Ada.Numerics.Float_Random.Random (Seed));
      Ada.Text_IO.Put_Line (Messages'Image(Message));
   end Writer;
   Taks: array(Messages) of access Writer -- 3 Writer tasks will immediately run
     := (new Writer(Enjoy), new Writer(Rosetta), new Writer(Code));
begin
   null; -- the "environment task" doesn't need to do anything
end Concurrent_Hello;
