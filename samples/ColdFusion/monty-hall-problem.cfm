<cfscript>
    function runmontyhall(num_tests) {
	// number of wins when player switches after original selection
	switch_wins	= 0;
	// number of wins when players "sticks" with original selection
	stick_wins  = 0;
	// run all the tests
	for(i=1;i<=num_tests;i++) {
	    // unconditioned potential for selection of each door
	    doors 	= [0,0,0];
	    // winning door is randomly assigned...
	    winner 	= randrange(1,3);
	    // ...and actualized in the array of real doors
	    doors[winner] = 1;
	    // player chooses one of three doors
	    choice 	= randrange(1,3);
	    do {
		// monty randomly reveals a door...
		shown = randrange(1,3);
	    }
	    // ...but monty only reveals empty doors;
	    // he will not reveal the door that the player has choosen
	    // nor will he reveal the winning door
	    while(shown==choice || doors[shown]==1);
	    // when the door the player originally selected is the winner, the "stick" option gains a point
	    stick_wins  += doors[choice];
	    // to calculate the number of times the player would have won with a "switch", subtract the
	    // "value" of the chosen, "stuck-to" door from 1, the possible number of wins if the player
	    // chose and stuck with the winning door (1), the player would not have won by switching, so
	    // the value is 1-1=0 if the player chose and stuck with a losing door (0), the player would
	    // have won by switching, so the value is 1-0=1
	    switch_wins += 1-doors[choice];
	}
	// finally, simply run the percentages for each outcome
	stick_percentage	= (stick_wins/num_tests)*100;
	switch_percentage	= (switch_wins/num_tests)*100;
	writeoutput('Number of Tests:  ' & num_tests);
	writeoutput('<br />Stick Wins: ' & stick_wins & '  ['& stick_percentage &'%]');
	writeoutput('<br />Switch Wins: ' & switch_wins & '  ['& switch_percentage &'%]');
    }
    runmontyhall(10000);
</cfscript>
