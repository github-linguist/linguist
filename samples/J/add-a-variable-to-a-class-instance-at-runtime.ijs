   C=:<'exampleclass'         NB. this will be our class name
   V__C=: 0                   NB. ensure the class exists
   OBJ1=:conew 'exampleclass' NB. create an instance of our class
   OBJ2=:conew 'exampleclass' NB. create another instance
   V__OBJ1,V__OBJ2            NB. both of our instances exist
0
   W__OBJ1                    NB. instance does not have a W
|value error
   W__OBJ1=: 0                NB. here, we add a W to this instance
   W__OBJ1                    NB. this instance now has a W
0
   W__OBJ2                    NB. our other instance does not
|value error
