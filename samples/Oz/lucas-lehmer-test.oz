%% compile : ozc -x <file.oz>
functor
import
  Application
  System
define

  fun {Arg Idx Default}
    Cmd = {Application.getArgs plain}
    Len = {Length Cmd}
  in
    if Len < Idx then
      Default
    else
      {StringToInt {Nth Cmd Idx}}
    end
  end

  fun {LLtest N}
    Mp = {Pow 2 N} - 1
    fun {S K} X T
    in
      if K == 1 then 4
      else
        T = {S K-1}
        X = T * T - 2
        X mod Mp
      end
    end
  in
    if N == 2 then
      true
    else
      {S N-1} == 0
    end
  end

  proc {FindLL X}
    fun {Sieve Ls}
      case Ls of nil then nil
      [] X|Xs then
        fun {DIV M} M mod X \= 0  end
      in
        X|{Sieve {Filter Xs DIV}}
      end
    end
  in
    if {IsList X} then
      case X of nil then skip
      [] M|Ms then
        {System.printInfo "M"#M#" "}
        {FindLL Ms}
      end
    else
      {FindLL {Filter {Sieve 2|{List.number 3 X 2}} LLtest}}
    end
  end

  Num = {Arg 1 607}

  {FindLL Num}

  {Application.exit 0}
end
