with Ada.Directories;  use Ada.Directories;
with Ada.Text_IO;

procedure Test_Directory_Walk is
   procedure Walk (Name : String; Pattern : String) is
      procedure Print (Item : Directory_Entry_Type) is
      begin
         Ada.Text_IO.Put_Line (Full_Name (Item));
      end Print;
      procedure Walk (Item : Directory_Entry_Type) is
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Walk (Full_Name (Item), Pattern);
         end if;
      exception
         when Name_Error => null;
      end Walk;
   begin
      Search (Name, Pattern, (others => True), Print'Access);
      Search (Name, "", (Directory => True, others => False), Walk'Access);
   end Walk;
begin
   Walk (".", "*.adb");
end Test_Directory_Walk;
