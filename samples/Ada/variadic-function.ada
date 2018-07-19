with Ada.Strings.Unbounded, Ada.Text_IO;

procedure Variadic is

   subtype U_String is Ada.Strings.Unbounded.Unbounded_String;
   use type U_String;

   function "+"(S: String) return U_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-"(U: U_String) return String
     renames Ada.Strings.Unbounded.To_String;

   type Variadic_Array is array(Positive range <>) of U_String;

   procedure Print_Line(Params: Variadic_Array) is
   begin
      for I in Params'Range loop
         Ada.Text_IO.Put(-Params(I));
         if I < Params'Last then
            Ada.Text_IO.Put(" ");
         end if;
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Line;

begin
   Print_Line((+"Mary", +"had", +"a", +"little", +"lamb.")); -- print five strings
   Print_Line((1 => +"Rosetta Code is cooool!")); -- print one string
end;
