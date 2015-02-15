with Ada.Text_IO;

procedure Partial_Function_Application is

   type Sequence is array(Positive range <>) of Integer;

   -- declare a function FS with a generic parameter F and a normal parameter S
   generic
      with function F(I: Integer) return Integer; -- generic parameter
   function FS (S: Sequence) return Sequence;

   -- define FS
   function FS (S: Sequence) return Sequence is
      Result: Sequence(S'First .. S'Last);
   begin
      for Idx in S'Range loop
         Result(Idx) := F(S(Idx));
      end loop;
      return Result;
   end FS;

   -- define functions F1 and F2
   function F1(I: Integer) return Integer is
   begin
      return 2*I;
   end F1;

   function F2(I: Integer) return Integer is
   begin
      return I**2;
   end F2;

   -- instantiate the function FS by F1 and F2 (partially apply F1 and F2 to FS)
   function FSF1 is new FS(F1);
   function FSF2 is new FS(F2);

   procedure Print(S: Sequence) is
   begin
      for Idx in S'Range loop
         Ada.Text_IO.Put(Integer'Image(S(Idx)));
      end loop;
      Ada.Text_IO.New_Line;
   end Print;

begin
   Print(FSF1((0,1,2,3)));
   Print(FSF2((0,1,2,3)));
   Print(FSF1((2,4,6,8)));
   Print(FSF2((2,4,6,8)));
end Partial_Function_Application;
