declare
  Target = "METHINKS IT IS LIKE A WEASEL"
  C = 100
  MutateRate = 5 %% percent

  proc {Main}
     X0 = {MakeN {Length Target} RandomChar}
  in
     for Xi in {Iterate Evolve X0} break:Break do
        {System.showInfo Xi}
        if Xi == Target then {Break} end
     end
  end

  fun {Evolve Xi}
     Copies = {MakeN C fun {$} {Mutate Xi} end}
  in
     {FoldL Copies MaxByFitness Xi}
  end

  fun {Mutate Xs}
     {Map Xs
      fun {$ X}
         if {OS.rand} mod 100 < MutateRate then {RandomChar}
         else X
         end
      end}
  end

  fun {MaxByFitness A B}
     if {Fitness B} > {Fitness A} then B else A end
  end

  fun {Fitness Candidate}
     {Length {Filter {List.zip Candidate Target Value.'=='} Id}}
  end

  Alphabet = & |{List.number &A &Z 1}
  fun {RandomChar}
     I = {OS.rand} mod {Length Alphabet} + 1
  in
     {Nth Alphabet I}
  end

  %% General purpose helpers

  fun {Id X} X end

  fun {MakeN N F}
     Xs = {List.make N}
  in
     {ForAll Xs F}
     Xs
  end

  fun lazy {Iterate F X}
     X|{Iterate F {F X}}
  end
in
  {Main}
