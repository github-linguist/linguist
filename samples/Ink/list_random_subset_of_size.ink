/*
	Returns a randomised subset of items from a list, up to a given size.

	Returns the empty list () if the source list is empty, and the complete list if it runs out of items to pick.

	Dependencies: 

		Requires "pop_random".

	Usage: 

		LIST fruitBowl = (apple), (banana), (melon)

		I put into my bag: {list_random_subset_of_size(fruitBowl, 2)}. 



*/

=== function list_random_subset_of_size(sourceList, n) 
    { n > 0:
        ~ temp el = pop_random(sourceList) 
        { el: 
            ~ return el + list_random_subset_of_size(sourceList, n-1)
        }
    }
    ~ return () 
    