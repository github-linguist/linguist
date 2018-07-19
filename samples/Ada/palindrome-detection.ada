function Palindrome (Text : String) return Boolean is
begin
   for Offset in 0..Text'Length / 2 - 1 loop
      if Text (Text'First + Offset) /= Text (Text'Last - Offset) then
         return False;
      end if;
   end loop;
   return True;
end Palindrome;
