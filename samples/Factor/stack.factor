 V{ 1 2 3 } {
 [ 6 swap push ]
 [ "hi" swap push ]
 [ "Vector is now: " write . ]
 [ "Let's pop it: " write pop . ]
 [ "Vector is now: " write . ]
 [ "Top is: " write last . ] } cleave

 Vector is now: V{ 1 2 3 6 "hi" }
 Let's pop it: "hi"
 Vector is now: V{ 1 2 3 6 }
 Top is: 6
