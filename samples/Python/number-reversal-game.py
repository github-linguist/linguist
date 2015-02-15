'''
number reversal game
    Given a jumbled list of the numbers 1 to 9
    Show the list.
    Ask the player how many digits from the left to reverse.
    Reverse those digits then ask again.
    until all the digits end up in ascending order.

'''

import random

print(__doc__)
data, trials = list('123456789'), 0
while data == sorted(data):
    random.shuffle(data)
while data != sorted(data):
    trials += 1
    flip = int(input('#%2i: LIST: %r Flip how many?: ' % (trials, ' '.join(data))))
    data[:flip] = reversed(data[:flip])

print('\nYou took %2i attempts to put the digits in order!' % trials)
