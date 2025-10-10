impl Game.play{
    raise NotImplementedError("Subclasses must implement this method.") ;
}


impl GuessTheNumberGame.init{
    self.attempts = 10;
    self.correct_number = random.randint(1, 10);
}


impl GuessTheNumberGame.play{
    while self.attempts > 0 {
        guess = input("Guess a number between 1 and 10: ");
        if guess.isdigit() {
            self.process_guess(int(guess));
        } else {
            print("That's not a valid number! Try again.");
        }
    }
    if not self.won {
        print("Sorry, you didn't guess the number. Better luck next time!");
    }
}


impl GuessTheNumberGame.process_guess(guess: int) {
    if guess > self.correct_number {
        print("Too high!");
        self.attempts -= 1;
    } elif guess < self.correct_number {
        print("Too low!");
        self.attempts -= 1;
    } else {
        print("Congratulations! You guessed correctly.");
        self.won = True;
        self.attempts = 0;
        return;
    }
    print(f"You have {self.attempts} attempts left.");
}