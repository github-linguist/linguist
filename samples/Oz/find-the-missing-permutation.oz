declare
  GivenPermutations =
  ["ABCD" "CABD" "ACDB" "DACB" "BCDA" "ACBD" "ADCB" "CDAB" "DABC" "BCAD" "CADB" "CDBA"
   "CBAD" "ABDC" "ADBC" "BDCA" "DCBA" "BACD" "BADC" "BDAC" "CBDA" "DBCA" "DCAB"]

  %% four distinct variables between "A" and "D":
  proc {Description Root}
     Root = {FD.list 4 &A#&D}
     {FD.distinct Root}
     {FD.distribute naiv Root}
  end

  AllPermutations = {SearchAll Description}
in
  for P in AllPermutations do
     if {Not {Member P GivenPermutations}} then
        {System.showInfo "Missing: "#P}
     end
  end
