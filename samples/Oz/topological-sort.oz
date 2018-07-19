declare
  Deps = unit(
            des_system_lib: [std synopsys std_cell_lib des_system_lib
                             dw02 dw01 ramlib ieee]
            dw01: [ieee dw01 dware gtech]
            dw02: [ieee dw02 dware]
            dw03: [std synopsys dware dw03 dw02 dw01 ieee gtech]
            dw04: [dw04 ieee dw01 dware gtech]
            dw05: [dw05 ieee dware]
            dw06: [dw06 ieee dware]
            dw07: [ieee dware]
            dware: [ieee dware]
            gtech: [ieee gtech]
            ramlib: [std ieee]
            std_cell_lib: [ieee std_cell_lib]
            synopsys:nil
            )

  %% Describe possible solutions
  proc {TopologicalOrder Solution}
     FullDeps = {Complete Deps}
  in
     %% The solution is a record that maps library names
     %% to finite domain variables.
     %% The smaller the value, the earlier it must be compiled
     Solution = {FD.record sol {Arity FullDeps} 1#{Width FullDeps}}
     %% for every lib on the left side
     {Record.forAllInd FullDeps
      proc {$ LibName Dependants}
         %% ... and every dependant on the right side
         for Dependant in Dependants do
            %% propagate compilation order
            if Dependant \= LibName then
               Solution.LibName >: Solution.Dependant
            end
         end
      end
     }
     %% enumerate solutions
     {FD.distribute naive Solution}
  end

  %% adds empty list of dependencies for libs that only occur on the right side
  fun {Complete Dep}
     AllLibs = {Nub {Record.foldL Dep Append nil}}
  in
     {Adjoin
      {List.toRecord unit {Map AllLibs fun {$ L} L#nil end}}
      Dep}
  end

  %% removes duplicates
  fun {Nub Xs}
     D = {Dictionary.new}
  in
     for X in Xs do D.X := unit end
     {Dictionary.keys D}
  end

  %% print grouped by parallelizable jobs
  proc {PrintSolution Sol}
     for I in 1..{Record.foldL Sol Value.max 1} do
        for Lib in {Arity {Record.filter Sol fun {$ X} X == I end}} do
           {System.printInfo Lib#" "}
        end
        {System.printInfo "\n"}
     end
  end

  fun {GetOrderedLibs Sol}
     {Map
      {Sort {Record.toListInd Sol} CompareSecond}
      SelectFirst}
  end
  fun {CompareSecond A B} A.2 < B.2 end
  fun {SelectFirst X} X.1 end
in
  case {SearchOne TopologicalOrder}
  of nil then {System.showInfo "Un-orderable."}
  [] [Sol] then
     {System.showInfo "A possible topological ordering: "}
     {ForAll {GetOrderedLibs Sol} System.showInfo}

     {System.showInfo "\nBONUS - grouped by parallelizable compile jobs:"}
     {PrintSolution Sol}
  end
