with Ada.Text_IO;

procedure Strip_ASCII is

   Full: String := 'a' & Character'Val(11) & 'b' & Character'Val(166) &
                   'c' & Character'Val(127) & Character'Val(203) &
                   Character'Val(202) & "de";
   -- 5 ordinary characters ('a' .. 'e')
   -- 2 control characters (11, 127); note that 11 is the "vertical tab"
   -- 3 extended characters (166, 203, 202)

   function Filter(S:     String;
                   From:  Character := ' ';
                   To:    Character := Character'Val(126);
                   Above: Character := Character'Val(127)) return String is
   begin
      if S'Length = 0 then
         return "";
      elsif (S(S'First) >= From and then S(S'First) <= To) or else S(S'First) > Above then
         return S(S'First) & Filter(S(S'First+1 .. S'Last), From, To, Above);
      else
         return Filter(S(S'First+1 .. S'Last), From, To, Above);
      end if;
   end Filter;

   procedure Put_Line(Text, S: String) is
   begin
      Ada.Text_IO.Put_Line(Text & " """ & S & """, Length:" & Integer'Image(S'Length));
   end Put_Line;

begin
   Put_Line("The full string :", Full);
   Put_Line("No Control Chars:", Filter(Full)); -- default values for From, To, and Above
   Put_Line("Neither_Extended:", Filter(Full, Above => Character'Last)); -- defaults for From and To
end Strip_ASCII;
