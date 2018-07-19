#!/usr/bin/python

from itertools import product, combinations
from random import sample

## Major constants
features = [ 'green purple red'.split(),
             'one two three'.split(),
             'oval diamond squiggle'.split(),
             'open striped solid'.split() ]

deck = list(product(list(range(3)), repeat=4))

dealt = 9

## Functions
def printcard(card):
    print(' '.join('%8s' % f[i] for f,i in zip(features, card)))

def getdeal(dealt=dealt):
    deal = sample(deck, dealt)
    return deal

def getsets(deal):
    good_feature_count = set([1, 3])
    sets = [ comb for comb in combinations(deal, 3)
             if all( [(len(set(feature)) in good_feature_count)
                     for feature in zip(*comb)]
                   ) ]
    return sets

def printit(deal, sets):
    print('Dealt %i cards:' % len(deal))
    for card in deal: printcard(card)
    print('\nFound %i sets:' % len(sets))
    for s in sets:
        for card in s: printcard(card)
        print('')

if __name__ == '__main__':
    while True:
        deal = getdeal()
        sets = getsets(deal)
        if len(sets) == dealt / 2:
           break
    printit(deal, sets)
