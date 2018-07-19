with Ada.Text_IO;

procedure Digital_Root is

   type Number is range 0 .. 2**63-1;
   type Number_Array is array(Positive range <>) of Number;
   type Base_Type is range 2 .. 16; -- any reasonable base to write down numbers

   procedure Compute(N: Number; Digital_Root, Persistence: out Number;
                     Base: Base_Type := 10) is

      function Digit_Sum(N: Number) return Number is
      begin
         if N < Number(Base) then
            return N;
         else
            return (N mod Number(Base)) + Digit_Sum(N / Number(Base));
         end if;
      end Digit_Sum;

   begin
      if N < Number(Base) then
         Digital_Root := N;
         Persistence := 0;
      else
         Compute(Digit_Sum(N), Digital_Root, Persistence, Base);
         Persistence := Persistence + 1;
      end if;
   end Compute;

   procedure Compute_And_Write(Values: Number_Array; Base: Base_Type := 10) is
      Root, Pers: Number;
      package NIO is new Ada.Text_IO.Integer_IO(Number);
   begin
      for I in Values'Range loop
         Compute(Values(I), Root, Pers, Base);
         NIO.Put(Values(I), Base => Integer(Base), Width => 12);
         Ada.Text_IO.Put(" has digital root ");
         NIO.Put(Root, Base => Integer(Base), Width => 0);
         Ada.Text_IO.Put(" and additive persistence"  & Number'Image(Pers));
         Ada.Text_IO.Put_Line(" (base" & Base_Type'Image(Base) & ").");
      end loop;
   end Compute_And_Write;

begin
   Compute_And_Write((961038, 923594037444, 670033, 448944221089));
   Compute_And_Write((16#7e0#, 16#14e344#, 16#12343210#), 16);
end Digital_Root;
