/*
	Returns a randomised subset of items from a list.

	Returns the empty list () if the source list is empty. Might return () anyway!

	Dependencies: 

		Requires "pop".

	Usage: 

		LIST fruitBowl = (apple), (banana), (melon)

		I put into my bag: {list_random_subset(fruitBowl)}. 



*/

=== function list_random_subset(sourceList) 
    ~ temp el = pop(sourceList) 
    {el:
        { RANDOM(0,1) == 0: 
            ~ el = () 
        }
        ~ return el + list_random_subset(sourceList) 
    }
    ~ return () 