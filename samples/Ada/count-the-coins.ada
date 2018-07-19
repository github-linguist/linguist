with Ada.Text_IO;

procedure Count_The_Coins is

   type Counter_Type is range 0 .. 2**63-1; -- works with gnat
   type Coin_List is array(Positive range <>) of Positive;

   function Count(Goal: Natural; Coins: Coin_List) return Counter_Type is
      Cnt: array(0 .. Goal) of Counter_Type := (0 => 1, others => 0);
      -- 0 => we already know one way to choose (no) coins that sum up to zero
      -- 1 .. Goal => we do not (yet) other ways to choose coins
   begin
      for C in Coins'Range loop
         for Amount in 1 .. Cnt'Last loop
            if Coins(C) <= Amount then
               Cnt(Amount) := Cnt(Amount) + Cnt(Amount-Coins(C));
               -- Amount-Coins(C) plus Coins(C) sums up to Amount;
            end if;
         end loop;
      end loop;
      return Cnt(Goal);
   end Count;

   procedure Print(C: Counter_Type) is
   begin
      Ada.Text_IO.Put_Line(Counter_Type'Image(C));
   end Print;

begin
   Print(Count(   1_00,          (25, 10, 5, 1)));
   Print(Count(1000_00, (100, 50, 25, 10, 5, 1)));
end Count_The_Coins;
