math.randomseed(os.time())
me_win=false
my_number=math.random(1,10)
while me_win==false do
	print "Guess my number from 1 to 10:"
	your_number = io.stdin:read'*l'
	if type(tonumber(your_number))=="number" then
		your_number=tonumber(your_number)
		if your_number>10 or your_number<1 then
			print "Your number was not between 1 and 10, try again."
		elseif your_number>my_number then
			print "Your number is greater than mine, try again."
		elseif your_number<my_number then
			print "Your number is smaller than mine, try again."
		elseif your_number==my_number then
			print "That was correct."
			me_win=true
		end
	else
		print "Your input was not a number, try again."
	end
end
