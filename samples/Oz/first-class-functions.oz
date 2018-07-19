declare

  fun {Compose F G}
     fun {$ X}
        {F {G X}}
     end
  end

  fun {Cube X} X*X*X end

  fun {CubeRoot X} {Number.pow X 1.0/3.0} end

in

  for
     F in [Float.sin  Float.cos  Cube]
     I in [Float.asin Float.acos CubeRoot]
  do
     {Show {{Compose I F} 0.5}}
  end
