declare
  Y = fun {$ F}
         {fun {$ X} {X X} end
          fun {$ X} {F fun {$ Z} {{X X} Z} end} end}
      end

  Fac = {Y fun {$ F}
              fun {$ N}
                 if N == 0 then 1 else N*{F N-1} end
              end
           end}

  Fib = {Y fun {$ F}
              fun {$ N}
                 case N of 0 then 0
                 [] 1 then 1
                 else {F N-1} + {F N-2}
                 end
              end
           end}
in
  {Show {Fac 5}}
  {Show {Fib 8}}
