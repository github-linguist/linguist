#!/usr/bin/python
from random import choice, randrange
from bisect import bisect
from collections import defaultdict

WHATBEATS = {   'paper' : 'scissors',
                'scissors' : 'rock',
                'rock' : 'paper'        }

ORDER = ('rock', 'paper', 'scissors')

CHOICEFREQUENCY = defaultdict(int)

def probChoice(choices, probabilities):
    total = sum(probabilities)
    prob_accumulator = 0
    accumulator = []
    for p in probabilities:
        prob_accumulator += p
        accumulator.append(prob_accumulator)
    r = randrange(total)
    bsct = bisect(accumulator, r)
    chc = choices[bsct]
    return chc

def checkWinner(a, b):
    if b == WHATBEATS[a]:
        return b
    elif a == WHATBEATS[b]:
        return a

    return None

def sanitizeChoice(a):
    # Drop it to lower-case
    return a.lower()

def registerPlayerChoice(choice):
    CHOICEFREQUENCY[choice] += 1

def getRandomChoice():
    if len(CHOICEFREQUENCY) == 0:
        return choice(ORDER)
    choices = CHOICEFREQUENCY.keys()
    probabilities = CHOICEFREQUENCY.values()
    return WHATBEATS[probChoice(choices, probabilities)]

while True:
    humanChoice = raw_input()
    humanChoice = sanitizeChoice(humanChoice)
    if humanChoice not in ORDER:
        continue

    compChoice = getRandomChoice()
    print "Computer picked", compChoice+",",

    # Don't register the player choice until after the computer has made
    # its choice.
    registerPlayerChoice(humanChoice)

    winner = checkWinner(humanChoice, compChoice)

    if winner == None:
        winner = "nobody"

    print winner, "wins!"
