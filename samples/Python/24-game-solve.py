'''
 The 24 Game Player

 Given any four digits in the range 1 to 9, which may have repetitions,
 Using just the +, -, *, and / operators; and the possible use of
 brackets, (), show how to make an answer of 24.

 An answer of "q"  will quit the game.
 An answer of "!"  will generate a new set of four digits.
 An answer of "!!" will ask you for a new set of four digits.
 An answer of "?"  will compute an expression for the current digits.

 Otherwise you are repeatedly asked for an expression until it evaluates to 24

 Note: you cannot form multiple digit numbers from the supplied digits,
 so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.

'''

from   __future__ import division, print_function
from   itertools  import permutations, combinations, product, \
                         chain
from   pprint     import pprint as pp
from   fractions  import Fraction as F
import random, ast, re
import sys

if sys.version_info[0] < 3:
    input = raw_input
    from itertools import izip_longest as zip_longest
else:
    from itertools import zip_longest


def choose4():
    'four random digits >0 as characters'
    return [str(random.randint(1,9)) for i in range(4)]

def ask4():
    'get four random digits >0 from the player'
    digits = ''
    while len(digits) != 4 or not all(d in '123456789' for d in digits):
        digits = input('Enter the digits to solve for: ')
        digits = ''.join(digits.strip().split())
    return list(digits)

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

def solve(digits):
    """\
    >>> for digits in '3246 4788 1111 123456 1127 3838'.split():
            solve(list(digits))


    Solution found: 2 + 3 * 6 + 4
    '2 + 3 * 6 + 4'
    Solution found: ( 4 + 7 - 8 ) * 8
    '( 4 + 7 - 8 ) * 8'
    No solution found for: 1 1 1 1
    '!'
    Solution found: 1 + 2 + 3 * ( 4 + 5 ) - 6
    '1 + 2 + 3 * ( 4 + 5 ) - 6'
    Solution found: ( 1 + 2 ) * ( 1 + 7 )
    '( 1 + 2 ) * ( 1 + 7 )'
    Solution found: 8 / ( 3 - 8 / 3 )
    '8 / ( 3 - 8 / 3 )'
    >>> """
    digilen = len(digits)
    # length of an exp without brackets
    exprlen = 2 * digilen - 1
    # permute all the digits
    digiperm = sorted(set(permutations(digits)))
    # All the possible operator combinations
    opcomb   = list(product('+-*/', repeat=digilen-1))
    # All the bracket insertion points:
    brackets = ( [()] + [(x,y)
                         for x in range(0, exprlen, 2)
                         for y in range(x+4, exprlen+2, 2)
                         if (x,y) != (0,exprlen+1)]
                 + [(0, 3+1, 4+2, 7+3)] ) # double brackets case
    for d in digiperm:
        for ops in opcomb:
            if '/' in ops:
                d2 = [('F(%s)' % i) for i in d] # Use Fractions for accuracy
            else:
                d2 = d
            ex = list(chain.from_iterable(zip_longest(d2, ops, fillvalue='')))
            for b in brackets:
                exp = ex[::]
                for insertpoint, bracket in zip(b, '()'*(len(b)//2)):
                    exp.insert(insertpoint, bracket)
                txt = ''.join(exp)
                try:
                    num = eval(txt)
                except ZeroDivisionError:
                    continue
                if num == 24:
                    if '/' in ops:
                        exp = [ (term if not term.startswith('F(') else term[2])
                               for term in exp ]
                    ans = ' '.join(exp).rstrip()
                    print ("Solution found:",ans)
                    return ans
    print ("No solution found for:", ' '.join(digits))
    return '!'

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
        if answer == '?':
            solve(digits)
            answer = '!'
        if answer.lower() == 'q':
            break
        if answer == '!':
            digits = choose4()
            trial = 0
            print ("\nNew digits:", ' '.join(digits))
            continue
        if answer == '!!':
            digits = ask4()
            trial = 0
            print ("\nNew digits:", ' '.join(digits))
            continue
        if not chk:
            print ("The input '%s' was wonky!" % answer)
        else:
            if '/' in answer:
                # Use Fractions for accuracy in divisions
                answer = ''.join( (('F(%s)' % char) if char in '123456789' else char)
                                  for char in answer )
            ans = eval(answer)
            print (" = ", ans)
            if ans == 24:
                print ("Thats right!")
    print ("Thank you and goodbye")

main()
