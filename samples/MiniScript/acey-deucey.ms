// Source: https://github.com/JoeStrout/miniscript/blob/master/MiniScript-cpp/demo/acey-deucey.ms
// Acey Deucey card game
// (Text-based version)

// Define some common stuff that makes this code work both
// in Mini Micro, and in command-line MiniScript.
if version.hostName == "Mini Micro" then
	clear
	chaChing = file.loadSound("/sys/sounds/cha-ching.wav")
	hit = file.loadSound("/sys/sounds/hit.wav")
else
	// For command-line MiniScript, make dummy mock-ups 
	// for the Sound class and our two sounds.
	Sound = {}
	Sound.play = null
	chaChing = new Sound
	hit = new Sound
end if

// Print the welcome message.
print "Welcome to Acey Deucey!"
print "On each round, you ante $2."
print "You are then dealt two cards."
print "Choose how much to bet, then one more card is dealt:"
print "  If the new card falls between the first two, you win 2X your bet!"
print "  If the new card falls outside the first two, you lose your bet."
print "  If the new card matches either of the first two cards exactly, you lose 2X!"
print

// Initialize our money and card values.
// (We use the Unicode code points for the suit characters.)
money = 100
cardValues = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "J", "Q", "K"]
suits = [char(9827), char(9830), char(9829), char(9824)] 

// Initialize the deck with all combinations of value and suit.
deck = []
for cv in cardValues
	for suit in suits
		deck.push cv + suit
	end for
end for
deck.shuffle
discard = []

// Function to deal a card from our deck,
// leaving it on the discard pile.
deal = function
	if deck.len == 0 then
		globals.deck = discard
		globals.discard = []
		deck.shuffle
	end if
	discard.push deck.pop
	return discard[-1]
end function

// Calculate the "rank" of a card.
cardRank = function(card, aceHigh=false)
	card = card[0]
	if card == "A" then return 1 + 12*aceHigh
	return cardValues.indexOf(card) + 1
end function

// Get the user's bet.  Make sure it's valid.
getBet = function
	while true
		betStr = input("Enter bet (0 to skip, Q to quit): ").upper
		if betStr == "Q" then return betStr
		if val(betStr) < 0 then
			print "A negative bet?  Nice try!"
		else if val(betStr) > money then
			print "You only have $" + money + "."
		else
			return betStr
		end if
	end while
end function

// Main loop.
while money > 2
	globals.money = money - 2
	print
	print "You have $" + money + "."
	wait
	card1 = deal
	rank1 = cardRank(card1, false)
	card2 = deal
	rank2 = cardRank(card2, true)
	print "  Card 1: " + card1 + "   (Rank: " + rank1 + ")"
	wait 0.5 
	print "  Card 2: " + card2 + "   (Rank: " + rank2 + ")"
	wait 0.5
	if rank1 == rank2 then
		print "Matched pair: ante lost."
		wait
		continue
	end if
	
	betStr = getBet
	if betStr == "Q" then break
	bet = val(betStr)
	if bet <= 0 then
		print "Chicken!"
		continue
	end if
	card3 = deal
	rank3 = cardRank(card3)
	print "  Card 3: " + card3 + "   (Rank: " + rank3 + ")"
	wait
	if rank3 == rank1 or rank3 == rank2 then
		print "Ohhh!  Double loss!"
		hit.play
		globals.money = money - bet * 2
	else if rank3 < rank1 and rank3 < rank2 then
		print "Too low!"
		globals.money = money - bet
	else if rank3 > rank1 and rank3 > rank2 then
		print "Too high!"
		globals.money = money - bet
	else
		print "You win!"
		chaChing.play
		globals.money = money + bet
	end if
	wait
end while

// Print the result.
if money == 0 then print "You are totally broke."
if money > 0 then print "You end the day with $" + money + "."
if money < 0 then print "You owe the bank $" + (-money) + "!"
print "Thanks for playing!"


