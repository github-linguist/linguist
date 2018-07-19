procedure Empty_String is

   function Is_Empty(S: String) return Boolean is
   begin
      return S = ""; -- test that S is empty
   end Is_Empty;

   Empty: String := ""; -- Assign empty string
   XXXXX: String := "Not Empty";

begin
   if (not Is_Empty(Empty)) or Is_Empty(XXXXX) then
      raise Program_Error with "something went wrong very very badly!!!";
   end if;
end Empty_String;
