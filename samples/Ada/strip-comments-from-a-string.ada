with Ada.Text_IO;
procedure Program is
   Comment_Characters : String := "#;";
begin
   loop
      declare
	 Line : String := Ada.Text_IO.Get_Line;
      begin
	 exit when Line'Length = 0;
	 Outer_Loop : for I in Line'Range loop
	    for J in Comment_Characters'Range loop
	       if Comment_Characters(J) = Line(I) then
		  Ada.Text_IO.Put_Line(Line(Line'First .. I - 1));
		  exit Outer_Loop;
	       end if;
	    end loop;
	 end loop Outer_Loop;
      end;
   end loop;
end Program;
