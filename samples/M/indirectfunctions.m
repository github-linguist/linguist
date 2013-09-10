start ; exercise
 set ^car("make")="toyota"
 set ^car("model")="corolla"
 set ^car("mileage")="$$compute^mileage"

 write !,"Regular computation",!
 write "make = ",^car("make"),!
 write "model = ",^car("model"),!
 write "mileage = ",@^car("mileage")@(150,4),!

 write !,"Pesimist computation",!
 set ^car("mileage")="$$computepesimist^mileage"
 write "make = ",^car("make"),!
 write "model = ",^car("model"),!
 write "mileage = ",@^car("mileage")@(150,4),!

 write !,"Optimist computation",!
 set ^car("mileage")="$$computeoptimist^mileage"
 write "make = ",^car("make"),!
 write "model = ",^car("model"),!
 write "mileage = ",@^car("mileage")@(150,4),!

