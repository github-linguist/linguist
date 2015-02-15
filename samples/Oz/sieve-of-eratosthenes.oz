declare
  fun {Sieve N}
     S = {Array.new 2 N true}
     M = {Float.toInt {Sqrt {Int.toFloat N}}}
  in
     for I in 2..M do
	if S.I then
	   for J in I*I..N;I do
	      S.J := false
	   end
	end
     end
     S
  end

  fun {Primes N}
     S = {Sieve N}
  in
     for I in 2..N collect:C do
	if S.I then {C I} end
     end
  end
in
  {Show {Primes 30}}
