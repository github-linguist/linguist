with Ada.Text_IO;

procedure Integers_In_English is

   type Spellable is range -999_999_999_999_999_999..999_999_999_999_999_999;
   function Spell (N : Spellable) return String is
      function Twenty (N : Spellable) return String is
      begin
         case N mod 20 is
            when  0 => return "zero";
            when  1 => return "one";
            when  2 => return "two";
            when  3 => return "three";
            when  4 => return "four";
            when  5 => return "five";
            when  6 => return "six";
            when  7 => return "seven";
            when  8 => return "eight";
            when  9 => return "nine";
            when 10 => return "ten";
            when 11 => return "eleven";
            when 12 => return "twelve";
            when 13 => return "thirteen";
            when 14 => return "fourteen";
            when 15 => return "fifteen";
            when 16 => return "sixteen";
            when 17 => return "seventeen";
            when 18 => return "eighteen";
            when others => return "nineteen";
         end case;
      end Twenty;

      function Decade (N : Spellable) return String is
      begin
         case N mod 10 is
            when 2 => return "twenty";
            when 3 => return "thirty";
            when 4 => return "forty";
            when 5 => return "fifty";
            when 6 => return "sixty";
            when 7 => return "seventy";
            when 8 => return "eighty";
            when others => return "ninety";
         end case;
      end Decade;

      function Hundred (N : Spellable) return String is
      begin
         if N < 20 then
            return Twenty (N);
         elsif 0 = N mod 10 then
            return Decade (N / 10 mod 10);
         else
            return Decade (N / 10) & '-' & Twenty (N mod 10);
         end if;
      end Hundred;

      function Thousand (N : Spellable) return String is
      begin
         if N < 100 then
            return Hundred (N);
         elsif 0 = N mod 100 then
            return Twenty (N / 100) & " hundred";
         else
            return Twenty (N / 100) & " hundred and " & Hundred (N mod 100);
         end if;
      end Thousand;

      function Triplet
               (  N     : Spellable;
                  Order : Spellable;
                  Name  : String;
                  Rest  : not null access function (N : Spellable) return String
               )  return String is
         High : Spellable := N / Order;
         Low  : Spellable := N mod Order;
      begin
         if High = 0 then
            return Rest (Low);
         elsif Low = 0 then
            return Thousand (High) & ' ' & Name;
         else
            return Thousand (High) & ' ' & Name & ", " & Rest (Low);
         end if;
      end Triplet;

      function Million (N : Spellable) return String is
      begin
         return Triplet (N, 10**3, "thousand", Thousand'Access);
      end Million;

      function Milliard (N : Spellable) return String is
      begin
         return Triplet (N, 10**6, "million", Million'Access);
      end Milliard;

      function Billion (N : Spellable) return String is
      begin
         return Triplet (N, 10**9, "milliard", Milliard'Access);
      end Billion;

      function Billiard (N : Spellable) return String is
      begin
         return Triplet (N, 10**12, "billion", Billion'Access);
      end Billiard;

   begin
      if N < 0 then
         return "negative " & Spell(-N);
      else
        return Triplet (N, 10**15, "billiard", Billiard'Access);
      end if;
   end Spell;

   procedure Spell_And_Print(N: Spellable) is
      Number: constant String := Spellable'Image(N);
      Spaces: constant String(1 .. 20) := (others => ' '); -- 20 * ' '
   begin
      Ada.Text_IO.Put_Line(Spaces(Spaces'First .. Spaces'Last-Number'Length)
                             & Number & ' ' & Spell(N));
   end Spell_And_Print;

   Samples: constant array (Natural range <>) of Spellable
     := (99, 300, 310, 1_501, 12_609, 512_609, 43_112_609, 77_000_112_609,
         2_000_000_000_100, 999_999_999_999_999_999,
         0, -99, -1501, -77_000_112_609, -123_456_789_987_654_321);

begin
   for I in Samples'Range loop
      Spell_And_Print(Samples(I));
   end loop;
end Integers_In_English;
