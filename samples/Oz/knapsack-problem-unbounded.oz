declare
  proc {Knapsack Sol}
     solution(panacea:P = {FD.decl}
              ichor:  I = {FD.decl}
              gold:   G = {FD.decl} ) = Sol
  in
                                           {Show 0#Sol}
      3 * P +  2 * I + 20 * G =<: 250      {Show 1#Sol}
     25 * P + 15 * I +  2 * G =<: 250      {Show 2#Sol}
     {FD.distribute naive Sol}             {Show d#Sol}
  end

  fun {Value solution(panacea:P ichor:I gold:G)}
     3000 * P + 1800 * I + 2500 * G
  end

  {System.showInfo "Search:"}
  [Best] = {SearchBest Knapsack proc {$ Old New}
                                   {Value Old} <: {Value New}
                                end}
in
  {System.showInfo "\nResult:"}
  {Show Best}
  {System.showInfo "total value: "#{Value Best}}
