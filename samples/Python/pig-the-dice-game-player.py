#!/usr/bin/python3

'''
See: http://en.wikipedia.org/wiki/Pig_(dice)

This program scores, throws the dice, and plays for an N player game of Pig.

'''

from random import randint
from collections import namedtuple
import random
from pprint import pprint as pp
from collections import Counter


playercount = 2
maxscore = 100
maxgames = 100000


Game = namedtuple('Game', 'players, maxscore, rounds')
Round = namedtuple('Round', 'who, start, scores, safe')


class Player():
    def __init__(self, player_index):
        self.player_index = player_index

    def __repr__(self):
        return '%s(%i)' % (self.__class__.__name__, self.player_index)

    def __call__(self, safescore, scores, game):
        'Returns boolean True to roll again'
        pass

class RandPlay(Player):
    def __call__(self, safe, scores, game):
        'Returns random boolean choice of whether to roll again'
        return bool(random.randint(0, 1))

class RollTo20(Player):
    def __call__(self, safe, scores, game):
        'Roll again if this rounds score < 20'
        return (((sum(scores) + safe[self.player_index]) < maxscore)    # Haven't won yet
                and(sum(scores) < 20))                                  # Not at 20 this round

class Desparat(Player):
    def __call__(self, safe, scores, game):
        'Roll again if this rounds score < 20 or someone is within 20 of winning'
        return (((sum(scores) + safe[self.player_index]) < maxscore)    # Haven't won yet
                and( (sum(scores) < 20)                                 # Not at 20 this round
                     or max(safe) >= (maxscore - 20)))                  # Someone's close


def game__str__(self):
    'Pretty printer for Game class'
    return ("Game(players=%r, maxscore=%i,\n  rounds=[\n    %s\n  ])"
            % (self.players, self.maxscore,
               ',\n    '.join(repr(round) for round in self.rounds)))
Game.__str__ = game__str__


def winningorder(players, safescores):
    'Return (players in winning order, their scores)'
    return tuple(zip(*sorted(zip(players, safescores),
                            key=lambda x: x[1], reverse=True)))

def playpig(game):
    '''
    Plays the game of pig returning the players in winning order
    and their scores whilst updating argument game with the details of play.
    '''
    players, maxscore, rounds = game
    playercount = len(players)
    safescore = [0] * playercount   # Safe scores for each player
    player = 0                      # Who plays this round
    scores=[]                       # Individual scores this round

    while max(safescore) < maxscore:
        startscore = safescore[player]
        rolling = players[player](safescore, scores, game)
        if rolling:
            rolled = randint(1, 6)
            scores.append(rolled)
            if rolled == 1:
                # Bust!
                round = Round(who=players[player],
                              start=startscore,
                              scores=scores,
                              safe=safescore[player])
                rounds.append(round)
                scores, player = [], (player + 1) % playercount
        else:
            # Stick
            safescore[player] += sum(scores)
            round = Round(who=players[player],
                          start=startscore,
                          scores=scores,
                          safe=safescore[player])
            rounds.append(round)
            if safescore[player] >= maxscore:
                break
            scores, player = [], (player + 1) % playercount

    # return players in winning order and all scores
    return winningorder(players, safescore)

if __name__ == '__main__':
    game = Game(players=tuple(RandPlay(i) for i in range(playercount)),
                maxscore=20,
                rounds=[])
    print('ONE GAME')
    print('Winning order: %r; Respective scores: %r\n' % playpig(game))
    print(game)
    game = Game(players=tuple(RandPlay(i) for i in range(playercount)),
                maxscore=maxscore,
                rounds=[])
    algos = (RollTo20, RandPlay, Desparat)
    print('\n\nMULTIPLE STATISTICS using %r\n  for %i GAMES'
          % (', '.join(p.__name__ for p in algos), maxgames,))
    winners = Counter(repr(playpig(game._replace(players=tuple(random.choice(algos)(i)
                                                               for i in range(playercount)),
                                                 rounds=[]))[0])
                      for i in range(maxgames))
    print('  Players(position) winning on left; occurrences on right:\n    %s'
          % ',\n    '.join(str(w) for w in winners.most_common()))
