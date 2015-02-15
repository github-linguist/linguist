function rps()
	print("Welcome to Rock, paper, scissors! Go ahead and type your pick.\n
		r(ock), p(aper), or s(cissors)\n
		Enter 'q' to quit.\n>")
	comp_score = 0
	user_score = 0
	options = ["r","p","s"]
	new_pick() = options[rand(1:3)]
	i_win(m,y) = ((m == "r" && y == "s")|(m == "s" && y == "p")|(m == "p" && y == "r"))
	while true
		input = string(readline(STDIN)[1])
		input == "q" && break
		!ismatch(r"^[rps]",input) && begin print("Invalid guess: Please enter 'r', 'p', or 's'\n"); continue end
		answer = new_pick()
		if input == answer
			print("\nTie!\nScore still: \nyou $user_score\nme  $comp_score\n>")
			continue
		else
			i_win(answer,input) ? (comp_score += 1) : (user_score += 1)
			print(i_win(answer,input) ? "\nSorry you lose!\n" : "\nYou win!","Score: \nyou $user_score\nme  $comp_score\n>")
		end
	end
end
