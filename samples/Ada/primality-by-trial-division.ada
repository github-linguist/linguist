function Is_Prime(Item : Positive) return Boolean is
   Result : Boolean := True;
   Test : Natural;
begin
   if Item /= 2 and Item mod 2 = 0 then
      Result := False;
   else
      Test := 3;
      while Test < Integer(Sqrt(Float(Item))) loop
         if Item mod Test = 0 then
            Result := False;
            exit;
         end if;
         Test := Test + 2;
      end loop;
  end if;
  return Result;
end Is_Prime;
