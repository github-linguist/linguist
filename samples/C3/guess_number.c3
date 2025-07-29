module guess_number;
import std::io;
import std::math;
import std::time;

struct Game
{
	int answer;
	bool done;
	int guesses;
	int high;
}

faultdef NOT_AN_INT, FAILED_TO_READ;

int err_count = 0;

fn int? ask_guess(int high)
{
	io::printf("Guess a number between 1 and %d: ", high);
	String text = io::treadline() ?? FAILED_TO_READ?!;
	return text.to_int() ?? NOT_AN_INT?;
}

fn int? ask_guess_multi(int high)
 {
	while (true)
	{
		int? result = ask_guess(high);
		if (@catch(result) == NOT_AN_INT)
		{
		  io::printn("I didn't understand that.");
		  err_count++;
		  continue;
		}
		return result;
	}
}

fn void? Game.play(Game *game)
{
	while (!game.done)
	{
		int guess = ask_guess_multi(game.high)!;
		game.report(guess);
		game.update(guess);
	}
}

fn void Game.report(Game* game, int guess)
{
	String desc @noinit;
	switch
	{
		case guess < game.answer: desc = "too low";
		case guess > game.answer: desc = "too high";
		default: desc = "the answer";
	}
	io::printfn("%d is %s.\n", guess, desc);
}

fn void Game.update(Game *game, int guess)
{
	if (guess == game.answer) game.done = true;
	game.guesses++;
}

fn void main()
{
	int high = 100;
	int answer = rand(high) + 1;
	Game game = { .answer = answer, .high = high };
	(void)game.play();
	io::printfn("Finished in %d guesses.", game.guesses);
	io::printfn("Total input errors: %d.", err_count);
}