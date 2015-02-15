declare
  %% Create a list of employee records.
  Data = {Map
          [['Tyler Bennett' e10297 32000 d101]
           ['John Rappl' e21437 47000 d050]
           ['George Woltman' e00127 53500 d101]
           ['Adam Smith' e63535 18000 d202]
           ['Claire Buckman' e39876 27800 d202]
           ['David McClellan' e04242 41500 d101]
           ['Rich Holcomb' e01234 49500 d202]
           ['Nathan Adams' e41298 21900 d050]
           ['Richard Potter' e43128 15900 d101]
           ['David Motsinger' e27002 19250 d202]
           ['Tim Sampair' e03033 27000 d101]
           ['Kim Arlich' e10001 57000 d190]
           ['Timothy Grove' e16398 29900 d190]]

          fun {$ [Name Id Salary Department]}
             employee(name:Name id:Id salary:Salary department:Department)
          end}

  fun {TopEarners Employees N}
     {Record.map {GroupBy Employees department}
      fun {$ Employees}
	 {List.take
	  {Sort Employees CompareSalary}
	  N}
      end}
  end

  fun {CompareSalary E1 E2}
     E1.salary > E2.salary
  end

  %% Groups the records Xs by the value of feature F and returns
  %% the result as a record that maps values of F to list of records.
  fun {GroupBy Xs F}
     Groups = {Dictionary.new}
  in
     for X in Xs do
        Groups.(X.F) := X|{CondSelect Groups X.F nil}
     end
     {Dictionary.toRecord unit Groups}
  end
in
  {Inspect {TopEarners Data 3}}
