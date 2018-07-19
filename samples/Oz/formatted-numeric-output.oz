declare
  fun {PrintFloat X Prec}
     {Property.put 'print.floatPrecision' Prec}
     S = {Float.toString X}
  in
     {Append
      for I in 1..Prec-{Length S}+1 collect:C do {C &0} end
      S}
  end
in
  {System.showInfo {PrintFloat 7.125 8}}
