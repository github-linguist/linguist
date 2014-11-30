% You can get a lot of information about Oz by following theses links  :
% - http://mozart.github.io/
% - http://en.wikipedia.org/wiki/Oz_(programming_language)
% There is also a well known book that uses Oz for pedagogical reason :
% - http://mitpress.mit.edu/books/concepts-techniques-and-models-computer-programming
% And there are two courses on edX about 'Paradigms of Computer Programming' that also uses Oz for pedagogical reason :
% - https://www.edx.org/node/2751#.VHijtfl5OSo
% - https://www.edx.org/node/4436#.VHijzfl5OSo
%
% Here is an example of some code written with Oz.

declare
% Computes the sum of square of the N first integers.
fun {Sum N}
  local SumAux in
    fun {SumAux N Acc}
      if N==0 then Acc
      else
        {Sum N-1 Acc}
      end
    end
    {SumAux N 0}
  end
end

% Returns true if N is a prime and false otherwize
fun {Prime N}
  local PrimeAcc in
    fun {PrimeAcc N Acc}
	    if(N == 1) then false
	    elseif(Acc == 1) then true
	    else
	      if (N mod Acc) == 0 then false
	      else
	       {PrimeAcc N Acc-1}
	      end
	   end
    end
  {PrimeAcc N (N div 2)}
  end
end

% Reverse a list using cells and for loop (instead of recursivity)
fun {Reverse L}
  local RevList in
    RevList = {NewCell nil}
    for E in L do
      RevList := E|@RevList
    end
    @RevList
  end
end
