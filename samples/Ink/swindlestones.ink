LIST Dice = 
    MeA1 = 11, MeA2, MeA3, MeA4,
    MeB1 = 21, MeB2, MeB3, MeB4,
    MeC1 = 31, MeC2, MeC3, MeC4,
    MeD1 = 41, MeD2, MeD3, MeD4,
    MeE1 = 51, MeE2, MeE3, MeE4,
    ThemA1 = 61, ThemA2, ThemA3, ThemA4,
    ThemB1 = 71, ThemB2, ThemB3, ThemB4,
    ThemC1 = 81, ThemC2, ThemC3, ThemC4,
    ThemD1 = 91, ThemD2, ThemD3, ThemD4,
    ThemE1 = 101, ThemE2, ThemE3, ThemE4
    
LIST Players = Me = 10, Them  = 60    
    
VAR DiceCountMe = 5
VAR DiceCountThem = 5


CONST DEBUG_FIXED_RANDOM = false // true 

CONST DEBUG_AI_DECISIONS = false // true 

{DEBUG_FIXED_RANDOM:
    ~ SEED_RANDOM(3)
}

-> begin_game -> END 



/*
    Aux
*/

=== function pop(ref _list) 
    ~ temp el = LIST_MIN(_list) 
    ~ _list -= el
    ~ return el 



=== function came_from(-> x) 
    ~ return TURNS_SINCE(x) == 0



=== function print_number(x) 
~ x = INT(x) // cast to an int, since this function can only handle ints!
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

/*
    Dice
*/

=== function faceValue(dice) 
    ~ return LIST_VALUE(dice) mod 10


/*
    Printing
*/

=== function stateDiceFor(who) 
    ~ temp dice = diceForPlayer(who) 
    <b>{listDice(dice)}</b>

=== function listDice(dice)
    {_listDice(dice, 1)}
=== function _listDice(dice, val)
    ~ temp values = valuesIn(val, dice) 
    ~ temp count = LIST_COUNT(values)
    { count > 0:
        {print_number(count)} {val}{count > 1:s}
        ~ dice -= values
    }
    {dice:
        { count > 0:
            { countValuesIn(faceValue(LIST_MIN(dice)), dice) == LIST_COUNT(dice):
                <> and 
            - else:
                <>, 
            }
        }
        <> { _listDice(dice, val+1) } 
    }

/*
    Queries
*/

=== function countValuesFor(value, who) 
    ~ temp dice = diceForPlayer(who) 
    ~ return LIST_COUNT(valuesIn(value, dice))
    
=== function countValuesIn(value, dice)
    ~ return LIST_COUNT(valuesIn(value, dice))
    
=== function valuesIn(value, dice)
    ~ temp lowestDice = pop(dice)
    { lowestDice:
        ~ temp retVal = ()
        {faceValue(lowestDice) == value:
            ~ retVal = lowestDice
        }
        ~ return retVal + valuesIn(value, dice)
    }
    ~ return ()


    
    

    
=== function diceCountForPlayer(who) 
    { who :
    -   Me:     ~ return DiceCountMe 
    -   Them:   ~ return DiceCountThem 
    }
    
=== function diceForPlayer(who) 
    ~ return LIST_RANGE(Dice, LIST_VALUE(who), LIST_VALUE(who) + 10 * diceCountForPlayer(who))
    
/*
    Rolling
*/
    
=== function rollDice()
    ~ Dice = () 
    ~ rollDiceFor(Me, DiceCountMe)
    ~ rollDiceFor(Them, DiceCountThem) 
 //   [Me:     {diceForPlayer(Me) }  ]
 //   [Them:   {diceForPlayer(Them) }]

=== function rollDiceFor(who, diceNumber) 
    { diceNumber > 0: 
        ~ temp diceOffset = LIST_VALUE(who) + (diceNumber - 1) * 10 + RANDOM(1, 4)
        ~ Dice += Dice(diceOffset)
        ~ rollDiceFor(who, diceNumber - 1) 
    }
    
/*
    The current bet
*/

=== function stateBet(bet)
    {print_number(betCount(bet))} {betNumber(bet)}{betCount(bet) > 1:s}

=== function betNumber(bet)
    ~ return LIST_VALUE(bet) mod 10

=== function betCount(bet)
    ~ return FLOOR(LIST_VALUE(bet) / 10) 


=== function possibleBets() 
    ~ return LIST_RANGE(LIST_ALL(Dice), LIST_VALUE(lastBet) + 1, (DiceCountMe + DiceCountThem) * 10 + 4)
    
=== function possibleBetCounts() 
    ~ temp bets = possibleBets()
    ~ return getCountsFromBets(bets)

=== function getCountsFromBets(bets)
    ~ temp bet = pop(bets) 
    { bet: 
        ~ bet = Dice(betCount(bet) * 10 + 1) 
        ~ return bet + getCountsFromBets(bets)
    }
    ~ return () 

=== function possibleBetsForCount(c) 
    ~ return possibleBets() ^ LIST_RANGE(LIST_ALL(Dice), c * 10, c * 10 + 4)



=== function countDiceAtValue(diceToConsider, value) 
    ~ temp die = pop(diceToConsider) 
    { die: 
        ~ temp retValue = (faceValue(die) == value)
        ~ return retValue + countDiceAtValue(diceToConsider, value) 
    }
    ~ return 0 


/*
    Game Loop
*/

VAR firstTurn = Me

VAR lastBet = ()
    
== begin_game 
    You pull up a seat at the table. The Half-Orc opposite picks his teeth with a dagger. 
    'Ready?' he grumbles. He tosses you a stack of dice.
- (opts)
    * (whatis)   'What's the game?'[] you ask.
        'Swindlestones,' the Half-Orc replies. 'A game of luck and brains.' He chuckles. 'And <i>looks.</i>' 
        -> opts 
    *   { whatis } 'Tell me the rules.' 
        'You roll behind your hand. I roll behind mine. You say, "There are 2 ones on the table," or something like that. I call to say, no, I do not believe it - or I say something higher. Higher number, or more dice. When called, we see who is right. Loser loses dice. No dice, loser is loser.' 
        * *     'I understand.' 
                
        * *     'So the bids keep going up?' 
                The Half-Orc nods. 
        - -     'Is simple. Now, play.' 
                -> opts 
    +   [ Roll the dice ]
        -> main
    
=== main 

    ~ rollDice() 
    You {DiceCountMe == 1:roll the remaining dice|gather up your {print_number(DiceCountMe)} dice and throw them} behind your palm, getting { stateDiceFor(Me)  }.
    The Half-Orc rolls his {DiceCountThem > 1:{print_number(DiceCountThem)}} dice and {~chuckles|snorts}. 
    
    ~ resetAI()

    ~ lastBet = ()
    ~ Players = firstTurn
    
    
    
    ->turnstart
    
= turnchange 
    ~ Players = LIST_INVERT(Players) 
    -> turnstart
    
= turnstart 
    { Players: 
    -   Me:     -> my_turn 
    -   Them:   -> their_turn 
    } 
    


= my_turn
    { not came_from(-> their_turn):
        'Your bet first,' the Half-Orc grumbles. 
    }
    
    { not came_from(-> stateDiceFor):
        [ You have {stateDiceFor(Me) } ]
    }
    
- (top)
    
    { lastBet:
        +   [ Call! ] 
            'I call,' you declare. 
            
            -> call_last_bet(Me)
    }

    ~ temp bets = possibleBetCounts()
    {bets:
        -> bet_opts(bets, true) 
    }
    
= bet_opts(bets, countsOnly)
- (opts)
    ~ temp bet = pop(bets)
    {countsOnly:
        +   [ Bet {print_number(betCount(bet))} ...  ]
            ~ bets = possibleBetsForCount(betCount(bet)) 
            -> bet_opts(bets, false) 
    - else:
        +   [ Bet {stateBet(bet)} ]
            'I bet <b>{stateBet(bet)}</b>,' you declare.
            -> makeBet(bet) 
    }
    { bets: 
        -> opts 
    }
    { not countsOnly:
        +   [ BACK ]
            -> top
    }
    -> DONE 
    

        
    
= makeBet(bet) 
    ~ lastBet = bet 
    -> turnchange
    

= their_turn

    'Let's see now,' the Half-Orc murmurs, scratching his chin with a hooked nail.

       
    ~ temp newBet = ()
    
    -> filter_and_obtain_bet(  newBet ) -> 
    
    
    { not newBet: 
        -> he_calls
    
    - else:
        -> he_bets(newBet) 
    } 
    
= he_bets(newBet) 
    <> 'I bet <b>{stateBet(newBet)}</b>. <>
    
    { cycle:
    -   {shuffle:
        -   Now - you.
        -   Well? 
        -   You, now. 
        -   Next, you. 
        -   Your turn. 
        -   Now you speak. 
        }
    -   {shuffle:
        -   What say you? 
        -   Give up now, I think. 
        -   I have you, yes. 
        -   Too big for you, I think. 
        -
        -
        -
        }
    }
    <>'
    
    -> makeBet(newBet) 

    
 = he_calls
        <> 'I call.'
        
        -> call_last_bet(Them) 
          
  
 
= call_last_bet(who)
    // who is calling on who
    The dice are revealed. Alongside my { stateDiceFor(Me)  }, he has { stateDiceFor(Them)  }.
    
    ~ temp valuesInSet = countValuesIn(betNumber(lastBet), Dice)
    
    ~ temp betWasOkay = ( valuesInSet >= betCount(lastBet) )
    
    <> That puts {not betWasOkay:only} <b>{print_number(valuesInSet)} {betNumber(lastBet)}{valuesInSet>1:s }</b> on the table.
    
    
    
    ~ temp winner = ()
    
    {not betWasOkay:
        ~ winner = who
    - else: 
        ~ winner = LIST_INVERT(who)
    }
    
    
    -> resolve_round(winner)
 
   
 = resolve_round(winner) 
    { winner:
    - Me:   ~ DiceCountThem--
    - Them: ~ DiceCountMe--
    }
    
    {
    - DiceCountThem <= 0:
        -> end_game(Me)
    - DiceCountMe <= 0:
        -> end_game(Them)
    }
    
    
    
    { winner:
    - Me: 
        The Half-Orc grumbles with irritation, and he tosses one of his dice away. 
    - Them:
        The Half-Orc nods in deep satisfaction, as you push one of your dice away. 
    }
    
    +   [ Roll again ]
        
        ~ firstTurn = winner 
        -> main
    


/*
    AI
*/

=== end_game(winner) 
    { winner: 
    - Me:   -> you_win 
    - Them:     -> he_wins  
    } 
    
= you_win 
    You beam with pleasure as you relieve the Half-Orc of his gold. Naturally, he reaches for his sword...
    
    ->-> 
    
= he_wins 
    You toss your last dice, and the creature reaches across the table to scoop the pile of gold into his lap. 
    'I say you loser from moment you sit on chair,' he grumbles with delight. 'Loser face.'
    ->-> 


/*
    AI
*/



=== function findBetsUpTo(value, maxCount, bets)
    ~ temp bet = pop(bets) 
    { bet: 
        ~ temp retVal = () 
        { betNumber(bet) == value && betCount(bet) <= maxCount: 
            ~ retVal = bet 
        }
        ~ return retVal + findBetsUpTo(value, maxCount, bets)
    } 
    ~ return () 

VAR whatDiceDoWeThinkYouHave = ()

=== function resetAI()
    ~ whatDiceDoWeThinkYouHave = ()   
 
===  filter_and_obtain_bet(  ref newBet )

    {DEBUG_AI_DECISIONS:  [ he's got {stateDiceFor(Them) } ] } 

    ~ temp countOfLastBet = betCount(lastBet) 
    ~ temp valueOfLastBet = betNumber(lastBet) 

    { lastBet: 
        ~ temp iThinkYouHave = FLOOR(countOfLastBet / 2) + 1 
        ~ iThinkYouHave -= countValuesIn(valueOfLastBet, whatDiceDoWeThinkYouHave) 
        { iThinkYouHave: // we think you've got more than we thought. add 1 dice. 
            ~ whatDiceDoWeThinkYouHave += LIST_RANDOM(valuesIn ( valueOfLastBet, LIST_ALL(Dice) - whatDiceDoWeThinkYouHave ) )
        }
    }
    
    
    
    
    ~ temp aRandomValue = RANDOM(1, 4)

    ~ temp bets = possibleBets()
    
    ~ temp cannotCall = countValuesFor(betNumber(lastBet), Them) >= betCount(lastBet)
    
    ~ temp valuesIHave = countValuesFor(valueOfLastBet, Them)
    
    ~ temp uncertaintyInYourDice = MAX(0, DiceCountMe - LIST_COUNT(whatDiceDoWeThinkYouHave) )
    
    {DEBUG_AI_DECISIONS: [ He thinks you have {listDice(whatDiceDoWeThinkYouHave)}, with uncertainty {uncertaintyInYourDice} ]  }
    
    +   { valuesIHave + DiceCountMe < countOfLastBet } ->
        {DEBUG_AI_DECISIONS: [ you overbet; we know it ]  }
        // you overbet and we know it for sure
    
    
    +   {findBetsUpTo(4, countValuesFor(4, Them), bets) } {RANDOM(1, 5) >= 4 }  -> 
        {DEBUG_AI_DECISIONS: [ safe high 4 bet ] }
        ~ newBet = findBetsUpTo(4, countValuesFor(4, Them) , bets)
    
    +   { not lastBet }     -> 
        { DEBUG_AI_DECISIONS:  [ picking a first bet randomly ] }
        ~ temp myCount = countValuesFor(aRandomValue, Them) 
        ~ newBet = findBetsUpTo(aRandomValue, myCount + 1,  bets ) 
        
    
        
    +   { LIST_COUNT(whatDiceDoWeThinkYouHave) > DiceCountMe * 1.5 } {not cannotCall} -> 
        {DEBUG_AI_DECISIONS: [ we suspect you're overbetting ] }
        // your bets are all over the place. Call. 
    
    +   { RANDOM(1, 3) == 1 } 
        { DiceCountThem + countValuesIn(valueOfLastBet, whatDiceDoWeThinkYouHave) <= countOfLastBet + 1 }
        -> 
        {DEBUG_AI_DECISIONS: [ we're raising you ]  }
        ~ newBet =  findBetsUpTo(valueOfLastBet, countOfLastBet + 1, bets )
        
      
    +   { valuesIHave + countValuesIn(valueOfLastBet, whatDiceDoWeThinkYouHave) + FLOOR(uncertaintyInYourDice / 4) + 1 <  countOfLastBet }  {not cannotCall} ->  
       {DEBUG_AI_DECISIONS:  [ we suspect you dont' have the dice to back this up ] }
    
    +   {findBetsUpTo(3, countValuesFor(3, Them), bets) }  
        { countValuesIn(4 , whatDiceDoWeThinkYouHave )  == 0} 
        -> 
        {DEBUG_AI_DECISIONS:  [ we've got high 3s, and we don't think you have any 4s; pushing you ] }
        ~ newBet = findBetsUpTo(3, countValuesFor(3, Them) , bets)
       
    +   {findBetsUpTo(1, countValuesFor(1, Them) + countValuesIn(1, whatDiceDoWeThinkYouHave) + uncertaintyInYourDice  / 2, bets) }     -> 
        {DEBUG_AI_DECISIONS: [ default bet: sensible 1s ] } 
        ~ newBet = findBetsUpTo(1, countValuesFor(1, Them) + countValuesIn(1, whatDiceDoWeThinkYouHave)  + uncertaintyInYourDice  / 2, bets)

    +   {findBetsUpTo(3, countValuesFor(3, Them) + countValuesIn(3, whatDiceDoWeThinkYouHave)  + uncertaintyInYourDice  / 2, bets) }     -> 
        {DEBUG_AI_DECISIONS: [ default bet: sensible 3s ] }
        ~ newBet = findBetsUpTo(3, countValuesFor(3, Them) + countValuesIn(3, whatDiceDoWeThinkYouHave) + uncertaintyInYourDice  / 2, bets)
        
    +   {findBetsUpTo(2, countValuesFor(2, Them) + countValuesIn(2, whatDiceDoWeThinkYouHave)  + uncertaintyInYourDice  / 2, bets) }     -> 
        {DEBUG_AI_DECISIONS: [ default bet: sensible 2s ] }
        ~ newBet = findBetsUpTo(2, countValuesFor(2, Them) + countValuesIn(2, whatDiceDoWeThinkYouHave) + uncertaintyInYourDice  / 2, bets)
        
    +   {findBetsUpTo(4, countValuesFor(4, Them) + countValuesIn(4, whatDiceDoWeThinkYouHave)  + uncertaintyInYourDice  / 2, bets) }     -> 
       {DEBUG_AI_DECISIONS:  [ default bet: sensible 4s ] }
        ~ newBet = findBetsUpTo(4, countValuesFor(4, Them) + countValuesIn(4, whatDiceDoWeThinkYouHave) + uncertaintyInYourDice  / 2, bets)    
    
       
    +   -> 
        {DEBUG_AI_DECISIONS: [ look for a fallback bet ] }
        - - (makebet)
            ~ newBet = pop(bets)
            {DEBUG_AI_DECISIONS:  [ {newBet}:  { betCount(newBet)} <= {countValuesFor(betNumber(newBet), Them)} + {countValuesIn(betNumber(newBet), whatDiceDoWeThinkYouHave)} + {uncertaintyInYourDice} ] } 
            
        + + { betCount(newBet) <= countValuesFor(betNumber(newBet), Them) + countValuesIn(betNumber(newBet), whatDiceDoWeThinkYouHave) + uncertaintyInYourDice  } 
            {RANDOM(1, 5) >= 3 || cannotCall }  -> 
            { DEBUG_AI_DECISIONS: [ Try a risky higher bet ]  }
            ->-> 
            
            
        + + -> 
            { not bets: 
                { DEBUG_AI_DECISIONS: [ We're trapped. Call. ]  }
                ~ newBet = ()
                ->-> 
            }
            -> makebet 
            
        
        
    
    -   // ensure we only have a single bet
        ~ newBet = LIST_RANDOM(newBet)
        
        
        ->->
    
    
