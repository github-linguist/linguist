with Ada.Text_IO, Miller_Rabin;

procedure Nemesis is

   type Number is range 0 .. 2**40-1; -- sufficiently large for the task

   function Is_Prime(N: Number) return Boolean is
      package MR is new Miller_Rabin(Number); use MR;
   begin
      return MR.Is_Prime(N) = Probably_Prime;
   end Is_Prime;

begin
   for P1 in Number(2) .. 61 loop
      if Is_Prime(P1) then
         for H3 in Number(1) .. P1 loop
            declare
               G: Number := H3 + P1;
               P2, P3: Number;
            begin
               Inner:
               for D in 1 .. G-1 loop
                  if ((H3+P1) * (P1-1)) mod D = 0 and then
                    (-(P1 * P1)) mod H3 = D mod H3
                  then
                     P2 := 1 + ((P1-1) * G / D);
                     P3 := 1 +(P1*P2/H3);
                     if Is_Prime(P2) and then Is_Prime(P3)
                       and then (P2*P3) mod (P1-1) = 1
                     then
                       Ada.Text_IO.Put_Line
                        ( Number'Image(P1) & " *"   & Number'Image(P2) & " *" &
                          Number'Image(P3) & "  = " & Number'Image(P1*P2*P3) );
                     end if;
                  end if;
               end loop Inner;
            end;
         end loop;
      end if;
   end loop;
end Nemesis;
