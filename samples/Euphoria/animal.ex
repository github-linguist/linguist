--****
-- === animal.ex
-- Guess the Animal game. It learns as you play.

constant
	node_type = 1,
	DESCRIPTION = 1,
	ANIMAL = 2,
	question = 2,
	when_true = 3,
	when_false = 4

constant
	TRUE = 1,
	FALSE = 0

constant KEYBOARD = 0, SCREEN = 1

object database, p, prev_p, prev_answer

function Yes() -- returns TRUE if the answer is yes
	object answer
	
	answer = '?'
	while answer = '?' do
		answer = gets(KEYBOARD)
		if length(answer) > 0 then
			if answer[1] = 'y' or answer[1] = 'Y' then
				answer = TRUE
			elsif answer[1] = 'n' or answer[1] = 'N' then
				answer = FALSE
			else
				answer = '?'
			end if
		else
			answer = '?'
		end if
		if answer = '?' then
			puts(SCREEN, "\nPlease answer y or n: ")
		end if
	end while
	return answer
end function

procedure guess() -- narrows it down to one animal
	while TRUE do
		if database[p][node_type] = ANIMAL then
			return
		end if
		printf(SCREEN, "\n%s? ", { database[p][question] })
		prev_p = p
		if Yes() then
			p = database[p][when_true]
			prev_answer = when_true
		else
			p = database[p][when_false]
			prev_answer = when_false
		end if
	end while
end procedure

global procedure animal()
	-- updates database of animal information
	
	object new, new_question, correct, answer
	
	-- initial database:
	--           record type     text         Y  N
	database = { { DESCRIPTION, "\tCan it fly", 2, 3 },
		 { ANIMAL, "an eagle" },
		 { ANIMAL, "a dog" } }
	while TRUE do
		p = 1
		printf(SCREEN, "%s\n",
			 { "\nThink of an animal. Hit <cr> when you are ready, q to quit" })
		answer = gets(KEYBOARD)
		if length(answer) > 0 then
			if answer[1] = 'q' then
				return
			end if
		end if
		guess()
		printf(SCREEN, "\n\tIs it %s? ", { database[p][question] })
		if not Yes() then
			puts(SCREEN, "\nI give up. What was it? ")
			correct = gets(KEYBOARD)
			correct = correct[1 .. length(correct) - 1]
			database = append(database, { ANIMAL, correct })
			new = length(database)
			printf(SCREEN, "\n%s%s%s%s\n",
				 { "Please give me a question that would distinguish ",
					database[p][question], " from ", correct })
			new_question = '\t' & gets(KEYBOARD)
			new_question = new_question[1 .. length(new_question) - 1]
			if new_question[length(new_question)] = '?' then
				new_question = new_question[1 .. length(new_question) - 1]
			end if
			printf(SCREEN, "\nFor %s the answer would be: ", { correct })
			if Yes() then
				database = append(database, { DESCRIPTION, new_question, new, p })
			else
				database = append(database, { DESCRIPTION, new_question, p, new })
			end if
			database[prev_p][prev_answer] = length(database)
		end if
	end while
end procedure

animal()

