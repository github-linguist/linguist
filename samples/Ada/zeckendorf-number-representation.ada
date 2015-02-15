with Ada.Text_IO, Ada.Strings.Unbounded;

procedure Print_Zeck is

   function Zeck_Increment(Z: String) return String is
   begin
      if Z="" then
	 return "1";
      elsif Z(Z'Last) = '1' then
	 return Zeck_Increment(Z(Z'First .. Z'Last-1)) & '0';
      elsif Z(Z'Last-1) = '0' then
	 return Z(Z'First .. Z'Last-1) & '1';
      else -- Z has at least two digits and ends with "10"
	 return Zeck_Increment(Z(Z'First .. Z'Last-2)) & "00";
      end if;
   end Zeck_Increment;

   use Ada.Strings.Unbounded;
   Current: Unbounded_String := Null_Unbounded_String;

begin
   for I in 1 .. 20 loop
      Current := To_Unbounded_String(Zeck_Increment(To_String(Current)));
      Ada.Text_IO.Put(To_String(Current) & " ");
   end loop;
end Print_Zeck;
