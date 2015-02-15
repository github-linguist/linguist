with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

procedure Walk_Directory
            (Directory : in String := ".";
             Pattern   : in String := "") -- empty pattern = all file names/subdirectory names
is
   Search  : Search_Type;
   Dir_Ent : Directory_Entry_Type;
begin
   Start_Search (Search, Directory, Pattern);

   while More_Entries (Search) loop
      Get_Next_Entry (Search, Dir_Ent);
      Put_Line (Simple_Name (Dir_Ent));
   end loop;

   End_Search (Search);
end Walk_Directory;
