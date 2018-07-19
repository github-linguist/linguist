with Ada.Text_IO; use Ada.Text_IO;
procedure FreeCell is
   type State is mod 2**31;
   type Deck is array (0..51) of String(1..2);

   package Random is
      procedure Init(Seed: State);
      function Rand return State;
   end Random;
   package body Random is
      S : State := State'First;
      procedure Init(Seed: State) is begin S := Seed; end Init;
      function Rand return State is begin
         S := S * 214013 + 2531011;  return S / 2**16;
      end Rand;
   end Random;

   procedure Deal (num : State) is
      thedeck : Deck;  pick : State;
      Chars : constant String := "A23456789TJQKCDHS";
   begin
      for i in thedeck'Range loop
         thedeck(i):= Chars(i/4+1) & Chars(i mod 4 + 14);
      end loop;
      Random.Init(num);
      for i in 0..51 loop
         pick := Random.Rand mod State(52-i);
         Put(thedeck(Natural(pick))&' ');
         if (i+1) mod 8 = 0 then New_Line; end if;
         thedeck(Natural(pick)) := thedeck(51-i);
      end loop; New_Line;
   end Deal;

begin
   Deal(1);
   New_Line;
   Deal(617);
end FreeCell;
