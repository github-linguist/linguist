declare
  fun {Compose F G}
     fun {$ X}
        {F {G X}}
     end
  end

  SinAsin = {Compose Float.sin Float.asin}
in
  {Show {SinAsin 0.5}}
