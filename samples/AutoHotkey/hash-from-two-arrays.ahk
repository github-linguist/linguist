array1 := ["two", "three", "apple"]
array2 := [2, 3, "fruit"]
hash := {}
Loop % array1.maxIndex()
   hash[array1[A_Index]] := array2[A_Index]
MsgBox % hash["apple"] "`n" hash["two"]
