'Simple number guessing game'

import random

target, guess = random.randint(1, 10), 0
while target != guess:
    guess = int(input('Guess my number between 1 and 10 until you get it right: '))
print('Thats right!')
