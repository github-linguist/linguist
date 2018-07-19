with Ada.Text_IO;

procedure Remove_Characters is
   S: String := "upraisers";
   use Ada.Text_IO;
begin
   Put_Line("Full String:   """ & S & """");
   Put_Line("Without_First: """ & S(S'First+1 .. S'Last) & """");
   Put_Line("Without_Last:  """ & S(S'First   .. S'Last-1) & """");
   Put_Line("Without_Both:  """ & S(S'First+1 .. S'Last-1) & """");
end Remove_Characters;
