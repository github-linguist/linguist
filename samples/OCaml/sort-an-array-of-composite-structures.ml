# let people = [("Joe", 12); ("Bob", 8); ("Alice", 9); ("Harry", 2)];;
val people : (string * int) list =
  [("Joe", 12); ("Bob", 8); ("Alice", 9); ("Harry", 2)]
# let sortedPeopleByVal = List.sort (fun (_, v1) (_, v2) -> compare v1 v2) people;;
val sortedPeopleByVal : (string * int) list =
  [("Harry", 2); ("Bob", 8); ("Alice", 9); ("Joe", 12)]
