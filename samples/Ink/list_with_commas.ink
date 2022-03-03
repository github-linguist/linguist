/*
	Takes a list and prints it out, using commas. 

	Dependenices: 

		This function relies on the "pop" function. 

	Usage: 

		LIST fruitBowl = (apples), (bananas), (oranges)

		The fruit bowl contains {list_with_commas(fruitBowl)}.
*/

=== function list_with_commas(list)
	{ list:
		{_list_with_commas(list, LIST_COUNT(list))}
	}

=== function _list_with_commas(list, n)
	{pop(list)}{ n > 1:{n == 2: and |, }{_list_with_commas(list, n-1)}}

	