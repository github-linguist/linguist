a = RunThrough["curl -u JohnDoe:Password https://www.example.com", 1]
For[ i=0, i < Length[a] , i++, SomeFunction[a]]
