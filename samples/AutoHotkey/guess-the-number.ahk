Random, rand, 1, 10  ; This stores a number between 1 and 10 in the var rand using the Mersenne Twister
msgbox I am thinking of a number between 1 and 10.

loop
{
	InputBox, guess, Guess the number, Type in a number from 1 to 10
		If (guess = rand)
		{
			msgbox Well Guessed!
			Break ; exits loop
		}
		Else
			Msgbox Try again.
}
