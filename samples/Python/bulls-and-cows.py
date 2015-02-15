'''
 Bulls and cows. A game pre-dating, and similar to, Mastermind.
'''

import random

digits = '123456789'
size = 4
chosen = ''.join(random.sample(digits,size))
#print chosen # Debug
print '''I have chosen a number from %s unique digits from 1 to 9 arranged in a random order.
You need to input a %i digit, unique digit number as a guess at what I have chosen''' % (size, size)
guesses = 0
while True:
    guesses += 1
    while True:
        # get a good guess
        guess = raw_input('\nNext guess [%i]: ' % guesses).strip()
        if len(guess) == size and \
           all(char in digits for char in guess) \
           and len(set(guess)) == size:
            break
        print "Problem, try again. You need to enter %i unique digits from 1 to 9" % size
    if guess == chosen:
        print '\nCongratulations you guessed correctly in',guesses,'attempts'
        break
    bulls = cows = 0
    for i in range(size):
        if guess[i] == chosen[i]:
            bulls += 1
        elif guess[i] in chosen:
            cows += 1
    print '  %i Bulls\n  %i Cows' % (bulls, cows)
