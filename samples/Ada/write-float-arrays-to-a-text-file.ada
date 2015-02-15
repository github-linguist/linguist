with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Write_Float_Array is
   type Float_Array is array (1..4) of Float;
   procedure Write_Columns
             (  File   : File_Type;
                X      : Float_Array;
                Y      : Float_Array;
                X_Pres : Natural := 3;
                Y_Pres : Natural := 5
             ) is
   begin
      for I in Float_Array'range loop
         Put (File => File, Item => X(I), Fore => 1, Aft => X_Pres - 1);
         Put (File, " ");
         Put (File => File, Item => Y(I), Fore => 1, Aft => Y_Pres - 1);
         New_Line (File);
      end loop;
   end Write_Columns;

   File : File_Type;
   X : Float_Array := (1.0, 2.0, 3.0, 1.0e11);
   Y : Float_Array;
begin
   Put ("Tell us the file name to write:");
   Create (File, Out_File, Get_Line);
   for I in Float_Array'range loop
      Y(I) := Sqrt (X (I));
   end loop;
   Write_columns (File, X, Y);
   Close (File);
end Write_Float_Array;
