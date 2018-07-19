declare
  fun {Factors N}
     Sqr = {Float.toInt {Sqrt {Int.toFloat N}}}

     Fs = for X in 1..Sqr append:App do
             if N mod X == 0 then
                CoFactor = N div X
             in
                if CoFactor == X then %% avoid duplicate factor
                   {App [X]}          %% when N is a square number
                else
                   {App [X CoFactor]}
                end
             end
          end
  in
     {Sort Fs Value.'<'}
  end
in
  {Show {Factors 53}}
