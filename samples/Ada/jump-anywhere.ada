procedure Goto_Test is
begin

   Stuff;
   goto The_Mother_Ship; -- You can do this if you really must!
   Stuff;
   if condition then
      Stuff;
   <<Jail>>
      Stuff;
   end if;
   Stuff;

   -- Ada does not permit any of the following
   goto Jail;
   goto The_Sewer;
   goto The_Morgue;

   Stuff;
   case condition is
      when Arm1 =>
         Stuff;
         goto The_Gutter; -- Cant do this either
         Stuff;
      when Arm2 =>
         Stuff;
      <<The_Gutter>>
         Stuff;
      <<The_Sewer>>
         Stuff;
   end case;

   Stuff;
   for I in Something'Range loop
      Stuff;
   <<The_Morgue>>
      if You_Are_In_Trouble then
         goto The_Mother_Ship;
         -- This is the usual use of a goto.
      end if;
      Stuff;
   end loop;

   Stuff;
<<The_Mother_Ship>>
   Stuff;

end Goto_Test;
