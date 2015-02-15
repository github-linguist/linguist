'''
 The 24 Game

 Given any four digits in the range 1 to 9, which may have repetitions,
 Using just the +, -, *, and / operators; and the possible use of
 brackets, (), show how to make an answer of 24.

 An answer of "q" will quit the game.
 An answer of "!" will generate a new set of four digits.
 Otherwise you are repeatedly asked for an expression until it evaluates to 24

 Note: you cannot form multiple digit numbers from the supplied digits,
 so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.

'''

from __future__ import division, print_function
import random, ast, re
import sys

if sys.version_info[0] < 3: input = raw_input

def choose4():
    'four random digits >0 as characters'
    return [str(random.randint(1,9)) for i in range(4)]

def welcome(digits):
    print (__doc__)
    print ("Your four digits: " + ' '.join(digits))

def check(answer, digits):
    allowed = set('() +-*/\t'+''.join(digits))
    ok = all(ch in allowed for ch in answer) and \
         all(digits.count(dig) == answer.count(dig) for dig in set(digits)) \
         and not re.search('\d\d', answer)
    if ok:
        try:
            ast.parse(answer)
        except:
            ok = False
    return ok

def main():
    digits = choose4()
    welcome(digits)
    trial = 0
    answer = ''
    chk = ans = False
    while not (chk and ans == 24):
        trial +=1
        answer = input("Expression %i: " % trial)
        chk = check(answer, digits)
        if answer.lower() == 'q':
            break
        if answer == '!':
            digits = choose4()
            print ("New digits:", ' '.join(digits))
            continue
        if not chk:
            print ("The input '%s' was wonky!" % answer)
        else:
            ans = eval(answer)
            print (" = ", ans)
            if ans == 24:
                print ("Thats right!")
    print ("Thank you and goodbye")
main()
