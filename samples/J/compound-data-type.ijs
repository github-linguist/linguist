   NB.  Create a "Point" class
   coclass'Point'

   NB. Define its constuctor
   create =: 3 : 0
     'X Y' =: y
   )

   NB.  Instantiate an instance (i.e. an object)
   cocurrent 'base'
   P =: 10 20 conew 'Point'

   NB.  Interrogate its members
   X__P
10
   Y__P
20
