function twentyfour()
 	function check(input)
 		d = ref(Int)
 		for i in input.args
 			if typeof(i) == Expr
 				c = check(i)
 				typeof(c) == String || append!(d,c)
 			elseif contains([:*,:/,:-,:+],i)
 				continue
 			elseif contains([1:9],i)
 				push!(d,i)
 				continue
 			elseif i > 9 || i < 1
 				d = "Sorry, $i is not allowed, please use only numbers between 1 and 9"
 			else
 				d = "Sorry, $i isn't allowed"
 			end
 		end
 		return d
 	end
 	new_digits() = [rand(1:9),rand(1:9),rand(1:9),rand(1:9)]
	answer = new_digits()
	print("The 24 Game\nYou will be given any four digits in the range 1 to 9, which may have repetitions.\n
		 Using just the +, -, *, and / operators show how to make an answer of 24.\n
		 Use parentheses, (), to ensure proper order of evaulation.\n
		 Enter 'n' fDouble>()

    while( scanner.hasNext() ) {
        if( scanner.hasNextInt() ) {
            var n = scanner.nextInt()

            // Make sure they're allowed to use n
            if( n or a new set of digits, and 'q' to quit. Good luck!\n
		 Here's your first 4 digits\n$(answer[1]) $(answer[2]) $(answer[3]) $(answer[4])\n
		 >")
	while true
		input = chomp(readline(STDIN))
		input == "q" && break
		if input == "n"
			answer = new_digits()
			print("\nLet's try again, go ahead and guess\n Here's your 4 digits\n$(answer[1]) $(answer[2]) $(answer[3]) $(answer[4])\n>")
			continue
		end
		input = try
					parse(input)
				catch
					print("I couldn't calculate your answer, please try again\n>");
					continue
				end
		c = check(input)
		cc = all([sum(i .== answer) == sum(i .== c) for i in answer])
        !cc && (print("Sorry, valid digits are \n$(answer[1]) $(answer[2]) $(answer[3]) $(answer[4])\n are allowed and all four must be used. Please try again\n>"); continue)
		if eval(input) == 24
			answer = new_digits()
			print("\nYou did it!\nLet's do another round, or enter 'q' to quit\n
				Here's your 4 digits\n$(answer[1]) $(answer[2]) $(answer[3]) $(answer[4])\n
		 		>")
			continue
		else
			print("\nSorry your answer calculates to $(eval(input))\nTry again\n>")
		end
	end
end
