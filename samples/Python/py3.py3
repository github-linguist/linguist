import random

guesses = 0

number = random.randint(1, 20)

print("Guess the number between 1 and 20! You have 6 tries.")
while guesses < 6:
    guess = int(input("Is it... "))

    if guess == number:
        print("Hooray! You guessed it right!")
        break
    elif guess < number:
        print("It's bigger...")
    elif guess > number:
        print("It's not so big.")
    guesses += 1
if guesses == 6:
    print("You've ran out of tries.")
