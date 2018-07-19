with Ada.Text_Io; use Ada.Text_Io;

procedure Roots_Of_Function is
   package Real_Io is new Ada.Text_Io.Float_Io(Long_Float);
   use Real_Io;

   function F(X : Long_Float) return Long_Float is
   begin
      return (X**3 - 3.0*X*X + 2.0*X);
   end F;

   Step  : constant Long_Float := 1.0E-6;
   Start : constant Long_Float := -1.0;
   Stop  : constant Long_Float := 3.0;
   Value : Long_Float := F(Start);
   Sign  : Boolean := Value > 0.0;
   X     : Long_Float := Start + Step;

begin
   if Value = 0.0 then
      Put("Root found at ");
      Put(Item => Start, Fore => 1, Aft => 6, Exp => 0);
      New_Line;
   end if;
   while X <= Stop loop
      Value := F(X);
      if (Value > 0.0) /= Sign then
         Put("Root found near ");
         Put(Item => X, Fore => 1, Aft => 6, Exp => 0);
         New_Line;
      elsif Value = 0.0 then
         Put("Root found at ");
         Put(Item => X, Fore => 1, Aft => 6, Exp => 0);
         New_Line;
      end if;
      Sign := Value > 0.0;
      X := X + Step;
   end loop;
end Roots_Of_Function;
