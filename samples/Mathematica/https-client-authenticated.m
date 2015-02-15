a = RunThrough["curl -E myCert.pem https://www.example.com", 1]
For[ i=0, i < Length[a] , i++, SomeFunction[a]]
