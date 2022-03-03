/*
	Tests if the flow has reached one divert more recently than another.

	If we have never reached the first divert, we return false. 
	If we have never reached the second divert, we return true. 

	This is especially useful for testing "have we done X this scene".

	Usage: 

	- (start_of_scene)
		"Welcome!"

	- (opts)	
		<- cough_politely(-> opts)

		*	{ seen_more_recently_than(-> cough_politely.cough, -> start_of_scene) }
			"Hello!"
		
		+	{ not seen_more_recently_than(-> cough_politely.cough, -> start_of_scene) }
			["Hello!"]
			I try to speak, but I can't get the words out!
			-> opts


		
	=== cough_politely(-> go_to)
		*	(cough) [Cough politely]
			I clear my throat. 
			-> go_to
		
*/

=== function seen_more_recently_than(-> link, -> marker)
	{ TURNS_SINCE(link) >= 0: 
        { TURNS_SINCE(marker) == -1: 
            ~ return true 
        } 
        ~ return TURNS_SINCE(link) < TURNS_SINCE(marker) 
    }
    ~ return false 

   