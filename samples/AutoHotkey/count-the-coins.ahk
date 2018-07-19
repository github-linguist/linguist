countChange(amount){
	return cc(amount, 4)
}

cc(amount, kindsOfCoins){
	if ( amount == 0 )
		return 1
	if ( amount < 0 ) || ( kindsOfCoins == 0 )
		return 0
	return cc(amount, kindsOfCoins-1)
	    +  cc(amount - firstDenomination(kindsOfCoins), kindsOfCoins)
}

firstDenomination(kindsOfCoins){
	return [1, 5, 10, 25][kindsOfCoins]
}
MsgBox % countChange(100)
