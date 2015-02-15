with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;
procedure EmptyDir is
   function Empty (path : String) return String is
      use Ada.Directories;
      result : String := "Is empty.";
      procedure check (ent : Directory_Entry_Type) is begin
         if Simple_Name (ent) /= "." and Simple_Name (ent) /= ".." then
            Empty.result := "Not empty";
         end if;
      end check;
   begin
      if not Exists (path) then return "Does not exist.";
      elsif Kind (path) /= Directory then return "Not a Directory.";
      end if;
      Search (path, "", Process => check'Access);
      return result;
   end Empty;
begin
   Put_Line (Empty ("."));
   Put_Line (Empty ("./empty"));
   Put_Line (Empty ("./emptydir.adb"));
   Put_Line (Empty ("./foobar"));
end EmptyDir;
