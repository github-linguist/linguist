note
	description: "[
					Monty Hall Problem as an Eiffel Solution
					
					1. Set the stage: Randomly place car and two goats behind doors 1, 2 and 3.
					2. Monty offers choice of doors --> Contestant will choose a random door or always one door.
					2a. Door has Goat - door remains closed
					2b. Door has Car - door remains closed
					3. Monty offers cash --> Contestant takes or refuses cash.
					3a. Takes cash: Contestant is Cash winner and door is revealed. Car Loser if car door revealed.
					3b. Refuses cash: Leads to offer to switch doors.
					4. Monty offers door switch --> Contestant chooses to stay or change.
					5. Door reveal: Contestant refused cash and did or did not door switch. Either way: Reveal!
					6. Winner and Loser based on door reveal of prize.
					
					Car Winner: Chooses car door
					Cash Winner: Chooses cash over any door
					Goat Loser: Chooses goat door
					Car Loser: Chooses cash over car door or switches from car door to goat door
					]"
	date: "$Date$"
	revision: "$Revision$"

class
	MH_APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Initialize Current.
		do
			play_lets_make_a_deal
		ensure
			played_1000_games: game_count = times_to_play
		end

feature {NONE} -- Implementation: Access

	live_contestant: attached like contestant
			-- Attached version of `contestant'
		do
			if attached contestant as al_contestant then
				Result := al_contestant
			else
				create Result
				check not_attached_contestant: False end
			end
		end

	contestant: detachable TUPLE [first_door_choice, second_door_choice: like door_number_anchor; takes_cash, switches_door: BOOLEAN]
			-- Contestant for Current.

	active_stage_door (a_door: like door_anchor): attached like door_anchor
			-- Attached version of `a_door'.
		do
			if attached a_door as al_door then
				Result := al_door
			else
				create Result
				check not_attached_door: False end
			end
		end

	door_1, door_2, door_3: like door_anchor
			-- Doors with prize names and flags for goat and open (revealed).

feature {NONE} -- Implementation: Status

	game_count, car_win_count, cash_win_count, car_loss_count, goat_loss_count, goat_avoidance_count: like counter_anchor
	switch_count, switch_win_count: like counter_anchor
	no_switch_count, no_switch_win_count: like counter_anchor
			-- Counts of games played, wins and losses based on car, cash or goat.

feature {NONE} -- Implementation: Basic Operations

	prepare_stage
			-- Prepare the stage in terms of what doors have what prizes.
		do
			inspect new_random_of (3)
			when 1 then
				door_1 := door_with_car
				door_2 := door_with_goat
				door_3 := door_with_goat
			when 2 then
				door_1 := door_with_goat
				door_2 := door_with_car
				door_3 := door_with_goat
			when 3 then
				door_1 := door_with_goat
				door_2 := door_with_goat
				door_3 := door_with_car
			end
			active_stage_door (door_1).number := 1
			active_stage_door (door_2).number := 2
			active_stage_door (door_3).number := 3
		ensure
			door_has_prize: not active_stage_door (door_1).is_goat or
							not active_stage_door (door_2).is_goat or
							not active_stage_door (door_3).is_goat
			consistent_door_numbers: active_stage_door (door_1).number = 1 and
										active_stage_door (door_2).number = 2 and
										active_stage_door (door_3).number = 3
		end

	door_number_having_prize: like door_number_anchor
			-- What door number has the car?
		do
			if not active_stage_door (door_1).is_goat then
				Result := 1
			elseif not active_stage_door (door_2).is_goat then
				Result := 2
			elseif not active_stage_door (door_3).is_goat then
				Result := 3
			else
				check prize_not_set: False end
			end
		ensure
			one_to_three: between_1_and_x_inclusive (3, Result)
		end

	door_with_car: attached like door_anchor
			-- Create a door with a car.
		do
			create Result
			Result.name := prize
		ensure
			not_empty: not Result.name.is_empty
			name_is_prize: Result.name.same_string (prize)
		end

	door_with_goat: attached like door_anchor
			-- Create a door with a goat
		do
			create Result
			Result.name := gag_gift
			Result.is_goat := True
		ensure
			not_empty: not Result.name.is_empty
			name_is_prize: Result.name.same_string (gag_gift)
			is_gag_gift: Result.is_goat
		end

	next_contestant: attached like live_contestant
			-- The next contestant on Let's Make a Deal!
		do
			create Result
			Result.first_door_choice := new_random_of (3)
			Result.second_door_choice := choose_another_door (Result.first_door_choice)
			Result.takes_cash := random_true_or_false
			if not Result.takes_cash then
				Result.switches_door := random_true_or_false
			end
		ensure
			choices_one_to_three: Result.first_door_choice <= 3 and Result.second_door_choice <= 3
			switch_door_implies_no_cash_taken: Result.switches_door implies not Result.takes_cash
		end

	choose_another_door (a_first_choice: like door_number_anchor): like door_number_anchor
			-- Make a choice from the remaining doors
		require
			one_to_three: between_1_and_x_inclusive (3, a_first_choice)
		do
			Result := new_random_of (3)
			from until Result /= a_first_choice
			loop
				Result := new_random_of (3)
			end
		ensure
			first_choice_not_second: a_first_choice /= Result
			result_one_to_three: between_1_and_x_inclusive (3, Result)
		end

	play_lets_make_a_deal
			-- Play the game 1000 times
		local
			l_car_win, l_car_loss, l_cash_win, l_goat_loss, l_goat_avoided: BOOLEAN
		do
			from
				game_count := 0
			invariant
				consistent_win_loss_counts: (game_count = (car_win_count + cash_win_count + goat_loss_count))
				consistent_loss_avoidance_counts: (game_count = (car_loss_count + goat_avoidance_count))
			until
				game_count >= times_to_play
			loop
				prepare_stage
				contestant := next_contestant
				l_cash_win := (live_contestant.takes_cash)

				l_car_win := (not l_cash_win and
								(not live_contestant.switches_door and live_contestant.first_door_choice = door_number_having_prize) or
								(live_contestant.switches_door and live_contestant.second_door_choice = door_number_having_prize))

				l_car_loss := (not live_contestant.switches_door and live_contestant.first_door_choice /= door_number_having_prize) or
									(live_contestant.switches_door and live_contestant.second_door_choice /= door_number_having_prize)

				l_goat_loss := (not l_car_win and not l_cash_win)

				l_goat_avoided := (not live_contestant.switches_door and live_contestant.first_door_choice = door_number_having_prize) or
									(live_contestant.switches_door and live_contestant.second_door_choice = door_number_having_prize)

				check consistent_goats: l_goat_loss implies not l_goat_avoided end
				check consistent_car_win: l_car_win implies not l_car_loss and not l_cash_win and not l_goat_loss end
				check consistent_cash_win: l_cash_win implies not l_car_win and not l_goat_loss end
				check consistent_goat_avoidance: l_goat_avoided implies (l_car_win or l_cash_win) and not l_goat_loss end
				check consistent_car_loss: l_car_loss implies l_cash_win or l_goat_loss end

				if l_car_win then car_win_count := car_win_count + 1 end
				if l_cash_win then cash_win_count := cash_win_count + 1 end
				if l_goat_loss then goat_loss_count := goat_loss_count + 1 end
				if l_car_loss then car_loss_count := car_loss_count + 1 end
				if l_goat_avoided then goat_avoidance_count := goat_avoidance_count + 1	end

				if live_contestant.switches_door then
					switch_count := switch_count + 1
					if l_car_win then
						switch_win_count := switch_win_count + 1
					end
				else -- if not live_contestant.takes_cash and not live_contestant.switches_door then
					no_switch_count := no_switch_count + 1
					if l_car_win or l_cash_win then
						no_switch_win_count := no_switch_win_count + 1
					end
				end


				game_count := game_count + 1
			end
			print ("%NCar Wins:%T%T " + car_win_count.out +
					"%NCash Wins:%T%T " + cash_win_count.out +
					"%NGoat Losses:%T%T " + goat_loss_count.out +
					"%N-----------------------------" +
					"%NTotal Win/Loss:%T%T" + (car_win_count + cash_win_count + goat_loss_count).out +
					"%N%N" +
					"%NCar Losses:%T%T " + car_loss_count.out +
					"%NGoats Avoided:%T%T " + goat_avoidance_count.out +
					"%N-----------------------------" +
					"%NTotal Loss/Avoid:%T" + (car_loss_count + goat_avoidance_count).out +
					"%N-----------------------------" +
					"%NStaying Count/Win:%T" + no_switch_count.out + "/" + no_switch_win_count.out + " = " + (no_switch_win_count / no_switch_count * 100).out + " %%" +
					"%NSwitch  Count/Win:%T" + switch_count.out + "/" + switch_win_count.out + " = " + (switch_win_count / switch_count * 100).out + " %%"
					)
		end

feature {NONE} -- Implementation: Random Numbers

	last_random: like random_number_anchor
			-- The last random number chosen.

	random_true_or_false: BOOLEAN
			-- A randome True or False
		do
			Result := new_random_of (2) = 2
		end

	new_random_of (a_number: like random_number_anchor): like door_number_anchor
			-- A random number from 1 to `a_number'.
		do
			Result := (new_random \\ a_number + 1).as_natural_8
		end

	new_random: like random_number_anchor
			-- Random integer
			-- Each call returns another random number.
		do
			random_sequence.forth
			Result := random_sequence.item
			last_random := Result
		ensure
			old_random_not_new: old last_random /= last_random
		end

	random_sequence: RANDOM
			-- Random sequence seeded from clock when called.
		attribute
			create Result.set_seed ((create {TIME}.make_now).milli_second)
		end

feature {NONE} -- Implementation: Constants

	times_to_play: NATURAL_16 = 1000
			-- Times to play the game.

	prize: STRING = "Car"
			-- Name of the prize

	gag_gift: STRING = "Goat"
			-- Name of the gag gift

	door_anchor: detachable TUPLE [number: like door_number_anchor; name: STRING; is_goat, is_open: BOOLEAN]
			-- Type anchor for door tuples.

	door_number_anchor: NATURAL_8
			-- Type anchor for door numbers.

	random_number_anchor: INTEGER
			-- Type anchor for random numbers.

	counter_anchor: NATURAL_16
			-- Type anchor for counters.

feature {NONE} -- Implementation: Contract Support

	between_1_and_x_inclusive (a_number, a_value: like door_number_anchor): BOOLEAN
			-- Is `a_value' between 1 and `a_number'?
		do
			Result := (a_value > 0) and (a_value <= a_number)
		end

end
