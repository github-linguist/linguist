[].asMap()                             # immutable, empty
["one" => 1, "two" => 2]               # immutable, 2 mappings
[].asMap().diverge()                   # mutable, empty
["one" => 2].diverge(String, float64)  # mutable, initial contents,
                                        #   typed (coerces to float)
