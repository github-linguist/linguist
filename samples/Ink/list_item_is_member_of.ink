/*
	Does a list item originate from a particular list? Returns false if testing ().

	Usage: 

	LIST Fruits = apple, banana, melon
	LIST Veggies = carrot, cucumber 

	~ temp x = apple
	I eat the {list_item_is_member_of(x, Fruits):fruit|vegetable}.

*/

=== function list_item_is_member_of(k, list) 
   	~ return k && LIST_ALL(list) ? k