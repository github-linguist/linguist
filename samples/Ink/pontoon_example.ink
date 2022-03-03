
/*

    This is a simplified version of the pontoon game from Overboard! 

    A deck of cards is properly simulated and dealt. The player can then bet on additional cards, before the AI characters (Carstairs, an English gentleman and card shark) will play.

    In the real game (www.inklestudios.com/overboard) conversation options are added between hands, and there a few other more nefarious strategies for tilting the game in your favour!

*/


-> play_game -> END 

/* ---------------------------------------------

    Functions and Definitions

--------------------------------------------- */

VAR myCards = ()
VAR hisCards = ()
VAR faceUpCards = ()

VAR money = 400 


VAR CstrsBank = 1000

 LIST PackOfCards = 
    A_Spades = 1, 2_Spades, 3_Spades, 4_Spades, 
    5_Spades, 6_Spades, 7_Spades, 8_Spades,
    9_Spades, 10_Spades, J_Spades, Q_Spades, K_Spades,
    A_Diamonds = 101 , 2_Diamonds, 3_Diamonds, 4_Diamonds, 
    5_Diamonds, 6_Diamonds, 7_Diamonds, 8_Diamonds,
    9_Diamonds, 10_Diamonds, J_Diamonds, Q_Diamonds, K_Diamonds,
    A_Hearts = 201, 2_Hearts, 3_Hearts, 4_Hearts, 
    5_Hearts, 6_Hearts, 7_Hearts, 8_Hearts,
    9_Hearts, 10_Hearts, J_Hearts, Q_Hearts, K_Hearts,
    A_Clubs = 301, 2_Clubs, 3_Clubs, 4_Clubs, 
    5_Clubs, 6_Clubs, 7_Clubs, 8_Clubs,
    9_Clubs, 10_Clubs, J_Clubs, Q_Clubs, K_Clubs

LIST Suits = Spades = 0, Diamonds, Hearts, Clubs

LIST Values = Ace = 1, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King



        
=== function suit(x) 
    ~ return Suits(INT(FLOOR(LIST_VALUE(x) / 100)))
    
=== function number(x) 
    ~ return Values(LIST_VALUE(x) mod 100)



=== function value(x) 
    ~ return MIN(LIST_VALUE(x) mod 100, 10)

=== function shuffle()
    ~ PackOfCards = LIST_ALL(PackOfCards)

=== function addSpecificCardOfValue(ref toHand, val, faceUp)
    ~ temp x = pullCardOfValue(val)
    ~ return addSpecificCard( toHand, x, faceUp)

=== function addSpecificCard(ref toHand, x, faceUp) 
    ~ toHand += x 
    {faceUp:
        ~ faceUpCards += x
    }
    ~ return x
    
=== function addCard(ref toHand, faceUp)
    ~ temp x = pullCardOfValue(LIST_ALL(Values))
    ~ temp retVal = addSpecificCard( toHand, x, faceUp)
    ~ return retVal 
    
    
=== function pullCardOfValue(valuesAllowed)
    ~ temp card = pop_random(PackOfCards) 
    { card: 
        { valuesAllowed !? number(card):
            ~ return pullCardOfValue(valuesAllowed)
        }
        ~ return card 
    }
    [ Error: couldn't find a card of value {valuesAllowed}! ]
    ~ shuffle()
    ~ return pullCardOfValue(valuesAllowed)
    
=== function nameCard(x) 
    {_nameCard(x, true) }
    
=== function _nameCard(x, allowVariants)
    ~ temp num = number(x)
    {allowVariants:
        { RANDOM(1, 3) == 1:
            a{(Eight, Ace) ? num :<>n} {num} in {suit(x)}
        - else:
            the {num} of {suit(x)}
        }
    - else: 
        {num} of {suit(x)}
    }
    
=== function printHandDescriptively(x, mine) 
    {printHand(faceUpCards ^ x)} face up
    ~ temp faceDownCards = x - faceUpCards
    {faceDownCards:
        <>, and <>
        { mine:
            {printHand(faceDownCards)}
        - else:
            {print_number(LIST_COUNT(faceDownCards))} <> more
        }
        <> {~{mine:hidden|}|face down|blind}
    }
    
=== function printHand(x)
    ~ _printHand(x)
=== function _printHand(x) 
    ~ temp y = pop(x) 
    {y:
        {nameCard(y)}
        {LIST_COUNT(x):
        - 0:    
            ~ return 
        - 1:
            <> and {_printHand(x)}
        - else:
            <>, {_printHand(x)}
        }
    }
    
== function listMyCards() 
    ~ _listOfCards(myCards)
== function _listOfCards(hand) 
    ~ temp y = pop(hand) 
    { y: 
        <>{_nameCard(y, false)}
        {hand:
            <><br>
            ~ _listOfCards(hand)
        }
    }
     

    
=== function isPontoon(x) 
    ~ return handContains(x, Ace) && ( handContains(x, King) || handContains(x, Queen) || handContains(x, Jack) ) && LIST_COUNT(x) == 2
    
=== function handContains(x, card) 
    ~ temp y = pop(x) 
    { y: 
        { number(y) == card: 
            ~ return true 
        - else:
            ~ return handContains(x, card)
        }
    }    
    ~ return false
    
=== function minTotalOfHand(x)
    ~ temp y = pop(x) 
    {y:
        ~ return minTotalOfHand(x) + value(y)
    }
    ~ return 0

=== function maxTotalOfHand(x)
    ~ temp minTot = minTotalOfHand(x)
    {handContains(x, Ace) && minTot <= 11:
        ~ return minTot + 10 
    - else: 
        ~ return minTot
    }
    
=== function sayTotalOfHand(x) 
    ~ temp minTot = minTotalOfHand(x)
    { shuffle:
    -   for a total of 
    -   total of 
    -   giving 
    -   making 
    }
    <> {print_number(minTot)} 
    { handContains(x, Ace)  && minTot <= 11:
        ~ temp max = maxTotalOfHand(x)
        <>, or {print_number(maxTotalOfHand(x))}
    }
=== function finalTotalOfHand(x) 
    { isPontoon(x): 
        pontoon
    - else:
        {print_number(maxTotalOfHand(x))} 
    }


=== function describeMyCards()
    { shuffle:
    -   V:      ... {printHandDescriptively(myCards, true)}. #thought
    - { shuffle:
        -   CARSTAIRS:  {~First {~card|out|up} {!for you} is|} 
            
        -   CARSTAIRS:  The lady {~has|gets} 
            
        }
        <> {nameCard(faceUpCards ^ myCards)}
        V:  ... and face down, {nameCard(myCards - faceUpCards)} ... #thought
    }
    V:      ... {sayTotalOfHand(myCards)} ... #thought




== function describePot(bet)
    { shuffle:
    -   CARSTAIRS:  The {~bet|stake|pot} is {print_number(bet)} pounds.
    -   CARSTAIRS:  {~That makes|{~There|That}'s} {print_number(bet)} pounds {~in the pot|on the table}. 
    }
    

/*------------------------------------------

    GAMEPLAY CONTENT LOOP

------------------------------------------*/

=== play_game

- (top_of_game)
    
    ~ temp startingMoney = money
    
    ~ myCards = () 
    
    ~ hisCards = () 
    ~ faceUpCards = ()

    ~ temp bet = 20
    
    { once:
    -   VO:     I throw two ten-pound notes onto the table. 
    -   V:  Twenty pounds. 
        CARSTAIRS:     The pot stands at twenty pounds.
    -   VO:     I toss in my ante. 
    
      
        
    }
    {
    
    - LIST_COUNT(PackOfCards) < 10:
        ~ shuffle()
        ~ temp plural = RANDOM(1,2)
        
        VO:         Carstairs {~collects together|gathers up} {plural:{~all|} the cards|the deck}, and {~riffles|shuffles} {plural:them|it} {~thoroughly|expertly|quickly|carelessly||} before dealing the first two cards.
        
    - else:
        
        VO:     Carstairs {~passes me|spins me|tosses over|deals out} {~{~an opening|a new} card|my first card} {~face up|} {~from the {~top of the|} deck|}.
    }
    ~ temp myNewCard = ()
    
    ~ myNewCard = addCard(myCards, true) 
    
    
    { shuffle:
    -   CARSTAIRS:  {~First {~card|out} is|} {nameCard(myNewCard)}. 
            
    -   CARSTAIRS:  The lady {~has|gets|receives} {nameCard(myNewCard)}.
    }
    
    ~ temp hisNewCard =  addCard(hisCards, true) 
    { stopping:
    -   CARSTAIRS:  And the dealer... gets {nameCard(hisNewCard)}.
        - 
        { shuffle:
        -   CARSTAIRS: And it's {nameCard(hisNewCard)} for me. 
                
        -   CARSTAIRS:  {~Dealer {~gets...|has}|And I have} {nameCard(hisNewCard)}.
        }
    }
    
    {once:
    -   CARSTAIRS:      You can fold, or make a bet to stay in.
    }

    ~ temp incr = 0
- (bet_opts)
    +   [ Fold ]
        
        V:  {~Pass|Fold}. 
        -> i_lost
        
    +   [ Bet 50  ]
        ~ incr =  50
    +   {money - bet < 200} [ Bet 100   ] 
        ~ incr = 100
    +   {money - bet >= 200} [ Bet higher... ] 
        + + {CHOICE_COUNT() < 2 }  {money - bet <= 300} [   Bet 100   ]
            ~ incr = 100
        + + {CHOICE_COUNT() < 2 } {money - bet <= 250} [   Bet 150    ]
            ~ incr =  150
        + + {CHOICE_COUNT() < 2 } [   Bet 200   ]
            ~ incr =  200
        + + {CHOICE_COUNT() < 2 } {money - bet >= 300} [   Bet 300   ]
            ~ incr = 300
        + + [ Bet lower... ] 
            -> bet_opts
                
                
-   
    { shuffle:
    -   V:  I put in {print_number(incr)} pounds {incr > 50: more}. 
    -   V:  I raise {print_number(incr)} pounds.
    }
    { incr >= 200: 
        
        { shuffle once:
        -   VO:     Carstairs raises an eyebrow. 
        -   CARSTAIRS:  Crikey. 
        -   CARSTAIRS:  Well, now. 
        -   CARSTAIRS:  Someone's feeling lucky. 
        }
    
    }
 -      ~ bet += incr
        
        { describePot(bet) }
        
        { shuffle:
        -   VO:     He {~hands|deals} {~me|out} a second card, face-down. 
        -   CARSTAIRS:  Here's your next card.
            { RANDOM(1, 2):
               VO:     He slides it across the table to me, face down. 
            }
        }
        
        {once:
        -   CARSTAIRS:  Take a look, don't let me see.
        }
        
        ~ myNewCard = addCard(myCards, false)  
        
        
        V:  ... {nameCard(myNewCard)}: {sayTotalOfHand(myCards)} ... #thought
        
        ~ addCard(hisCards , false) 
        
        { shuffle:
        -   VO:     He deals one more for himself, face down. 
        -   CARSTAIRS:  One more blind for me, too. 
        }
  
- (myplay) 

    { minTotalOfHand(myCards) > 21:
        { shuffle:
        -   V:  I'm bust. 
        -   V:  Damn.
        -   VO:     I {~toss|throw} my cards down. 
        }
        { i_lost mod 3 == 2:
            { shuffle: 
            -   V:  You're rigging this. 
            -   V:  How are you doing this? 
            -   V:  This can't be fair. 
            }
            { shuffle:
            -   CARSTAIRS:  I assure you I'm not! 
            -   CARSTAIRS:  I play the odds, Ma'am, not the player. 
            -   CARSTAIRS:  I promise you, I'm as square as they come!
            }
            
        
        }
        -> i_lost
    }
    { LIST_COUNT(myCards) == 5: 
        CARSTAIRS:  A five card trick! 
        CARSTAIRS:  That beats the same value on fewer cards.
    }
 
 - (check_for_burn)  
    { LIST_COUNT(myCards) == 2 && minTotalOfHand(myCards) == 13 && money - bet >= 20: 
        +   {came_from(-> burny)} 
            [ Burn again ] 
            -> burny 
        +   (burny) {not came_from(-> burny)} 
            [ Burn for twenty more ] 
            ~ bet += 20 
            V:  Burn. 
            >>> AUDIO CardCollectAndDealTwoCards
            VO:     Carstairs collects in the cards and deals two more.
            ~ faceUpCards -= myCards
            ~ myCards = () 
            ~ addCard(myCards, true)
            ~ addCard(myCards, false)
            V:      ... {printHandDescriptively(myCards, true)} ... #thought
            V:      ... {sayTotalOfHand(myCards)} ... #thought

            -> check_for_burn
            
        *   [ Keep them ] 
            -> bid_loop
    - else: 
        -> bid_loop   
    }
    -> DONE 
    
- (bid_loop)  

    { not seen_very_recently(->  describePot): 
        { describePot(bet) }
    }
    ~ temp gotTwentyOne = (maxTotalOfHand(myCards) == 21)
    {gotTwentyOne:
        {isPontoon(myCards):
            V:  ... It's a pontoon..!  #thought
        - else: 
            V:  ... Twenty-one!   #thought
        }
        
    }
    
    +   [ Stick {not gotTwentyOne: on {finalTotalOfHand(myCards)}} ]
        CARSTAIRS:  Final bet is {print_number(bet)} pounds. 
        -> hisplay_begins
        
    *   (gloat) {gotTwentyOne} [ Gloat ] 
        >>> AUDIO: V Chuckle 1
        V:  You're in trouble now, Mr Carstairs...
        CARSTAIRS:  Is that so?
        -> hisplay_begins
        
    *   {gotTwentyOne} [ Give nothing away ] 
        >>> AUDIO: V Clear Throat 1
        V:          Your turn, then.
        CARSTAIRS:  I take it you're sticking, then?
        -> hisplay_begins
        
    +   {not gotTwentyOne} [ Twist ] 
        { shuffle:
        -   V:  Twist. 
        -   V:  Another card. 
        -   V:  Give me another.
        -   V:  One more, face up.
        }
        ~ temp newUpCard = addCard(myCards, true)
        
        CARSTAIRS:  {nameCard(newUpCard)}.
        
        V:  ... {sayTotalOfHand(myCards)}. #thought
        -> myplay
        
    +   { (money - bet) >= 50 }  {not gotTwentyOne}
        [ Buy for fifty ]
        ~ bet += 50 
        ~ temp newDownCard = addCard(myCards, false)
        {shuffle:
        -   V:  Buy. 
        -   V:  I'll buy one. 
        -   V:  One more, face down.
        }
        {shuffle:
        -   CARSTAIRS:  The stake is now {print_number(bet)}. 
        -   CARSTAIRS:   {print_number(bet)} in the pot. 
        }
       
        { shuffle:
        -   VO:     Carstairs passes me another card, face-down. 
        -   CARSTAIRS:   Here's your card.
        }
       
        V:  ... {nameCard(newDownCard)}. #thought
        V:  ... {sayTotalOfHand(myCards)}. #thought
        -> myplay
        
- (hisplay_begins)  

    ~ faceUpCards += hisCards 
    { shuffle:
    -   CARSTAIRS:  Let's see what I have...
        CARSTAIRS:  {printHandDescriptively(hisCards, false)}.
    -   CARSTAIRS:  Dealer has... {printHandDescriptively(hisCards, false)}.
    }
    
    CARSTAIRS:  {sayTotalOfHand(hisCards)}.
 
- (hisplay_main)
    // AI plays 
    
    ~ temp hes_scared = seen_more_recently_than(-> gloat, -> top_of_game)
    
    ~ temp hisTotal = minTotalOfHand(hisCards) 
    
    { hisTotal > 21:  
        { shuffle:
        -   CARSTAIRS:  I'm bust!
        -   CARSTAIRS:  Too high! 
        -   CARSTAIRS:  No luck there!
        }
        -> i_won 
    }
    
    ~ temp hisMaxTotal = maxTotalOfHand(hisCards) 
    
    ~ temp yourVisibleTotal = maxTotalOfHand(myCards ^ faceUpCards)
    ~ temp yourBestTotal = 21 
    
    // edge case. You have ? - 3 - 5 => your best is 19.
    { LIST_COUNT(myCards - faceUpCards) == 1 && yourVisibleTotal < 10: 
        ~ yourBestTotal = 11 + yourVisibleTotal
    }
    
    +   {hisMaxTotal > yourBestTotal || (hisMaxTotal == yourBestTotal && LIST_COUNT(myCards) < 5)} ->
        - - (he_sticks)
            CARSTAIRS:  Dealer sticks on {finalTotalOfHand(hisCards)}.
            -> hisplayover
    +   { hisMaxTotal >= 18 && !handContains(hisCards, Ace)}   -> he_sticks
    
    +   { hisTotal == 10 || hisTotal == 11 } -> he_twists
    
    +   { hisMaxTotal <= 15 || (hisMaxTotal <= 17 && handContains(hisCards, Ace)) || (hisMaxTotal <= 18 && hes_scared) } -> 
        - - (he_twists)
            { shuffle:
            -   CARSTAIRS: I'll take another. 
            -    CARSTAIRS: Dealer twists. 
            -    CARSTAIRS: One more...
            }
            
            ~ temp newHisCard = addCard(hisCards, true)
            CARSTAIRS:  {nameCard(newHisCard)}, {sayTotalOfHand(hisCards)}.
            -> hisplay_main
        
    +   {RANDOM(1, 3) == 1} -> 
        -> he_sticks 
        
    +   -> he_twists
    
- (hisplayover) 
    
    ~ temp facedownCards = myCards - faceUpCards
    
- (dealoutcards)
    { pop(facedownCards):
        -> dealoutcards
    }
    
    
    ~ temp scoreDiff = maxTotalOfHand(myCards) - maxTotalOfHand(hisCards)
    { cycle:
    -   VO:     I lay my cards down. 
    -  VO:     I {~turn|flip} my cards {~face-up|over}. 
      -  
    }
    
    { cycle:
    -   V:  I've got {scoreDiff < 0:only} {finalTotalOfHand(myCards)}{scoreDiff==0:<> too}.
    -  V:      {finalTotalOfHand(myCards)}.
    }
    
    { 
    - scoreDiff > 0 && maxTotalOfHand(myCards) < 21: 
        {stopping:
        -   V:  I won? 
        -   {cycle: 
                - V:  I won. 
                - 
            }
        }
        -> i_won 
    - scoreDiff < 0: 
        CARSTAIRS:  Dealer wins! 
        -> i_lost 
    - scoreDiff == 0: 
        { LIST_COUNT(myCards) >= 5 && LIST_COUNT(hisCards) < 5: 
            CARSTAIRS:  Five card trick wins!
            -> i_won 
        }
        CARSTAIRS:  It's a draw. Dealer wins, I'm afraid.
        -> i_lost 
    }
    
    
- (i_won)
    ~ money += bet 
    ~ CstrsBank -= bet
    
    VO:     I collect up the money from the table. 
    { 
    - isPontoon(myCards): 
        CARSTAIRS:  And pontoon earns double. 
        ~ money += bet 
        ~ CstrsBank -= bet
        
        VO:     He counts out another {print_number(bet)} pounds. 
    - maxTotalOfHand(myCards) == 21 && LIST_COUNT(myCards) == 2:
        { once:
        -   CARSTAIRS:  But it's not a pontoon, I'm afraid. 
            CARSTAIRS:  Need a face card for that.
            
        }
    }
    
    { shuffle:
    -   VO:     I've now got {print_number(money)} pounds. 
    -   V:      ... I've now got {print_number(money)} pounds. 
    }  
    
    -> done 

- (i_lost)

    ~ money -= bet
    ~ CstrsBank += bet
    VO:     Carstairs {~takes|{~collects|scoops} {~up|}} the {~pot|stake|money {~{~off|from} the table|}} and gathers up the cards. 
    { money < 50: 
        V:  You've cleaned me out! 
        CARSTAIRS:  I'm sorry to hear that, Mrs V. 
        CARSTAIRS:  Thanks for the game. 
        
        
        VO:     He tucks his winnings into his waistcoat pocket and grins like an idiot.
        -> finished
    }
    { money >= startingMoney:
        { shuffle:
        -   VO:     I've still got {print_number(money)} pounds. 
        }
    - else: 
        { shuffle:
        -   V:      ... I'm down to {print_number(money)} pounds ... #thought
        -   V:     ... {print_number(money)} pounds left ...  #thought
        }
    }
    -> done 
    
- (done)

    ~ temp wasPontoon = isPontoon(myCards)
    ~ myCards = ()
    
    { CstrsBank <= 50:
        CARSTAIRS:  Well, you've cleaned me out of spending money, Mrs Villensey! 
        CARSTAIRS:  I must say; a much better show than your husband achieved. 
        -> finished
    }
    
    {
    - came_from(-> i_lost): 
        {shuffle: 
        -   CARSTAIRS:       Have you had enough? 
        -   CARSTAIRS:       Keep going?  
        -   CARSTAIRS:       Again?
        }
    - came_from(-> i_won): 
        { shuffle:
        -    CARSTAIRS:      Another round? 
        -    CARSTAIRS:      Again? 
        -    CARSTAIRS:      Another? 
        }
    - else:     
        { cycle:
        -   VO:     Carstairs {~has been squaring up|is fiddling with} the {~pack|deck}.
        -   VO:     Carstairs is shuffling idly. 
            ~ shuffle()
        }
        { shuffle: 
         -    CARSTAIRS:      Are we still playing?
         -    CARSTAIRS:      Another hand, Mrs Villensey?
        }
    }
    
 - (replay_opts)  
    
    +   [ Play another round ] 
        { 
        - money >= 250:
            { shuffle:
            -   V:  Hit me. 
                
            -   V:  Deal.
                
            -   V:  Let's try again.
                
            -   V:  Another!
                
            }
        - money >= 100:
            { shuffle:
            -   V:  I'll play another round.
                
            -   V:  I'll play a little more. 
                
            -   V:  I'm not finished yet.
            }
        -  money >= 70:
            { shuffle:
            -   V:  I can afford one more round.
            -   V:  I'd better be lucky this time! 
            }
        }
        -> top_of_game 
    
    
       
    +   [ Stop playing ] 
        {shuffle:
        -   V:  Perhaps later.  
        -   V:  Another time, perhaps. 
        }
            
    - (finished)
        ~ myCards = ()
        
        ->->


/*------------------------------------------

    STOCK FUNCTIONS
    
    These functions are all available from the ink snippet menu in inky 0.12.0 and above

------------------------------------------*/

/*
	Tests if the flow passes a particular gather on this turn.

	Usage: 

	- (welcome)
		"Welcome!"
	- (opts)
		*	{came_from(->welcome)}
			"Welcome to you!"
		*	"Er, what?"
			-> opts
		*	"Can we get on with it?"
		
*/

=== function came_from(-> x) 
    ~ return TURNS_SINCE(x) == 0

/*
	Tests if the flow passes a particular gather "very recently" - that is, within the last 3 turns.

	Usage: 

	- (welcome)
		"Welcome!"
	- (opts)
		*	{seen_very_recently(->welcome)}
			"Sorry, hello, yes."
		+	"Er, what?"
			-> opts
		*	"Can we get on with it?"
		
*/

=== function seen_very_recently(-> x)
    ~ return TURNS_SINCE(x) >= 0 && TURNS_SINCE(x) <= 3
    
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

   



/*
	Takes the bottom element from a list, and returns it, modifying the list.

	Returns the empty list () if the source list is empty.

	Usage: 

	LIST fruitBowl = (apple), (banana), (melon)

	I eat the {pop(fruitBowl)}. Now the bowl contains {fruitBowl}.

*/

=== function pop(ref _list) 
    ~ temp el = LIST_MIN(_list) 
    ~ _list -= el
    ~ return el 


/*
	Takes a random element from a list, and returns it, modifying the list.

	Returns the empty list () if the source list is empty.

	Usage: 

	LIST fruitBowl = (apple), (banana), (melon)

	I eat the {pop_random(fruitBowl)}. Now the bowl contains {fruitBowl}.

*/

=== function pop_random(ref _list) 
    ~ temp el = LIST_RANDOM(_list) 
    ~ _list -= el
    ~ return el 
    



/*
    Converts an integer between -1,000,000,000 and 1,000,000,000 into its printed equivalent.

    Usage: 

    There are {print_number(RANDOM(100000,10000000))} stars in the sky.

*/

=== function print_number(x) 
{
    - x >= 1000000:
        ~ temp k = x mod 1000000
        {print_number((x - k) / 1000000)} million{ k > 0:{k < 100: and|{x mod 100 != 0:<>,}} {print_number(k)}}
    - x >= 1000:
        ~ temp y = x mod 1000
        {print_number((x - y) / 1000)} thousand{ y > 0:{y < 100: and|{x mod 100 != 0:<>,}} {print_number(y)}}
    - x >= 100:
        ~ temp z = x mod 100
        {print_number((x - z) / 100)} hundred {z > 0:and {print_number(z)}}
    - x == 0:
        zero
    - x < 0: 
        minus {print_number(-1 * x)}
    - else:
        { x >= 20:
            { x / 10:
                - 2: twenty
                - 3: thirty
                - 4: forty
                - 5: fifty
                - 6: sixty
                - 7: seventy
                - 8: eighty
                - 9: ninety
            }
            { x mod 10 > 0:
                <>-<>
            }
        }
        { x < 10 || x > 20:
            { x mod 10:
                - 1: one
                - 2: two
                - 3: three
                - 4: four
                - 5: five
                - 6: six
                - 7: seven
                - 8: eight
                - 9: nine
            }
        - else:
            { x:
                - 10: ten
                - 11: eleven
                - 12: twelve
                - 13: thirteen
                - 14: fourteen
                - 15: fifteen
                - 16: sixteen
                - 17: seventeen
                - 18: eighteen
                - 19: nineteen
            }
        }
} 


