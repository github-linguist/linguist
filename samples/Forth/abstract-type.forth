include 4pp/lib/foos.4pp

:: X()
   class
     method: method1
     method: method2
   end-class {
     :method { ." Method 1 in X" cr } ; defines method1
   }
;

:: Y()
   extends X()
   end-extends {
     :method { ." Method 2 in Y" cr } ; defines method2
   }
;

: Main
  static Y() y
  y => method1
  y => method2
;

Main
