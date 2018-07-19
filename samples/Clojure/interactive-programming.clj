Clojure 1.1.0
user=> (defn f [s1 s2 sep] (str s1 sep sep s2))
#'user/f
user=> (f "Rosetta" "Code" ":")
"Rosetta::Code"
user=>
