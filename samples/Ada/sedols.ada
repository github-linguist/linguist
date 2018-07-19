with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_SEDOL is

   subtype SEDOL_String is String (1..6);
   type SEDOL_Sum is range 0..9;

   function Check (SEDOL : SEDOL_String) return SEDOL_Sum is
      Weight : constant array (SEDOL_String'Range) of Integer := (1,3,1,7,3,9);
      Sum    : Integer := 0;
      Item   : Integer;
   begin
      for Index in SEDOL'Range loop
         Item := Character'Pos (SEDOL (Index));
         case Item is
            when Character'Pos ('0')..Character'Pos ('9') =>
               Item := Item - Character'Pos ('0');
            when Character'Pos ('B')..Character'Pos ('D') |
                 Character'Pos ('F')..Character'Pos ('H') |
                 Character'Pos ('J')..Character'Pos ('N') |
                 Character'Pos ('P')..Character'Pos ('T') |
                 Character'Pos ('V')..Character'Pos ('Z') =>
               Item := Item - Character'Pos ('A') + 10;
            when others =>
               raise Constraint_Error;
         end case;
         Sum := Sum + Item * Weight (Index);
      end loop;
      return SEDOL_Sum ((-Sum) mod 10);
   end Check;

   Test : constant array (1..10) of SEDOL_String :=
             (  "710889", "B0YBKJ", "406566", "B0YBLH", "228276",
                "B0YBKL", "557910", "B0YBKR", "585284", "B0YBKT"
             );
begin
   for Index in Test'Range loop
      Put_Line (Test (Index) & Character'Val (Character'Pos ('0') + Check (Test (Index))));
   end loop;
end Test_SEDOL;
