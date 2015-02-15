for break:Break do
   R = {OS.rand} mod 20
in
   {Show R}
   if R == 10 then {Break}
   else {Show {OS.rand} mod 20}
   end
end
