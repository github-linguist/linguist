#!/usr/bin/python3

'''
See: http://en.wikipedia.org/wiki/Pig_(dice)

This program scores and throws the dice for a two player game of Pig

'''

from random import randint

playercount = 2
maxscore = 100
safescore = [0] * playercount
player = 0
score=0

while max(safescore) < maxscore:
    rolling = input("Player %i: (%i, %i) Rolling? (Y) "
                    % (player, safescore[player], score)).strip().lower() in {'yes', 'y', ''}
    if rolling:
        rolled = randint(1, 6)
        print('  Rolled %i' % rolled)
        if rolled == 1:
            print('  Bust! you lose %i but still keep your previous %i'
                  % (score, safescore[player]))
            score, player = 0, (player + 1) % playercount
        else:
            score += rolled
    else:
        safescore[player] += score
        if safescore[player] >= maxscore:
            break
        print('  Sticking with %i' % safescore[player])
        score, player = 0, (player + 1) % playercount

print('\nPlayer %i wins with a score of %i' %(player, safescore[player]))
