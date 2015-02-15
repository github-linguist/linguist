function wait(waittime)--wait function is used so that app will not quit immediately
  local timer = os.time()
  repeat until os.time() == timer + waittime
end


upperBound = 100
lowerBound = 0
print("Think of an integer between 1 to 100.")
print("I will try to guess it.")
while true do
	upper1 = upperBound+1
	upper2 = upperBound-1
	if upperBound == lowerBound or upper1 == lowerBound or upper2 == lowerBound or lowerBound > upperBound then--make sure player is not cheating
		io.write("You're cheating! I'm not playing anymore. Goodbye.")
		wait(3)
		break
	else
		Guess = math.floor((upperBound + lowerBound)/2)--guessing mechanism
		print("My guess is: "..Guess..". Is it too high, too low, or correct? (h/l/c)")
		input = io.read()

	if input == "h" then --higher
		upperBound = Guess
	elseif input == "l" then --lower
		lowerBound = Guess
	elseif input == "c" then --correct
		io.write("So I win? Thanks for playing with me.")
		wait(3)
		break
	else
		print("Invalid input. Please try again. ")
		end
	end
end
