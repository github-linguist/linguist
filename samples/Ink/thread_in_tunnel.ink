
/*
	Threads in a given flow as a tunnel, with a given location to tunnel back to. 

	If choices within this content are taken, they should end with a tunnel return (->->).

	Useful for "pasting in" the same block of optional content into multiple locations.

	Usage: 


	- (opts)
		<- thread_in_tunnel(-> eat_apple, -> opts)
		<- thread_in_tunnel(-> eat_banana, -> get_going)
		*	[ Leave hungry ]
			-> get_going

	=== get_going
		You leave. 
		-> END 

	=== eat_apple 
		*	[ Eat an apple ]
			You eat an apple. It doesn't help.
			->->

	=== eat_banana 
		*	[ Eat a banana ]
			You eat a banana. It's very satisfying.
			->->
		
		
*/

=== thread_in_tunnel(-> tunnel_to_run, -> place_to_return_to)

    ~ temp entryTurnChoice = TURNS()
    
    -> tunnel_to_run ->
 
 	// if the tunnel contained choices which were chosen, then the turn count will 
 	// have increased, so we should use the given return point to continue the flow.
    {entryTurnChoice != TURNS():
        -> place_to_return_to      
    }  

    // otherwise the given tunnel simply ran through, in which case we should treat
    // this as a side-thread, and close it down.
    -> DONE 