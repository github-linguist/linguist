List1 := [1,2,1,3,2]
List2 := [1,2,0,4,4,0,0,0]
MsgBox % order(List1, List2)

order(L1, L2){
	return L1.MaxIndex() < L2.MaxIndex()
}
