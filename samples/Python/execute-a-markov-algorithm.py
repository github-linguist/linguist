import re

def extractreplacements(grammar):
    return [ (matchobj.group('pat'), matchobj.group('repl'), bool(matchobj.group('term')))
                for matchobj in re.finditer(syntaxre, grammar)
                if matchobj.group('rule')]

def replace(text, replacements):
    while True:
        for pat, repl, term in replacements:
            if pat in text:
                text = text.replace(pat, repl, 1)
                if term:
                    return text
                break
        else:
            return text

syntaxre = r"""(?mx)
^(?:
  (?: (?P<comment> \# .* ) ) |
  (?: (?P<blank>   \s*  ) (?: \n | $ )  ) |
  (?: (?P<rule>    (?P<pat> .+? ) \s+ -> \s+ (?P<term> \.)? (?P<repl> .+) ) )
)$
"""

grammar1 = """\
# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule
"""

grammar2 = '''\
# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
'''

grammar3 = '''\
# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
'''

grammar4 = '''\
### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ ->
'''

grammar5 = '''\
# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11
'''

text1 = "I bought a B of As from T S."

text2 = "I bought a B of As W my Bgage from T S."

text3 = '_1111*11111_'

text4 = '000000A000000'


if __name__ == '__main__':
    assert replace(text1, extractreplacements(grammar1)) \
           == 'I bought a bag of apples from my brother.'
    assert replace(text1, extractreplacements(grammar2)) \
           == 'I bought a bag of apples from T shop.'
    # Stretch goals
    assert replace(text2, extractreplacements(grammar3)) \
           == 'I bought a bag of apples with my money from T shop.'
    assert replace(text3, extractreplacements(grammar4)) \
           == '11111111111111111111'
    assert replace(text4, extractreplacements(grammar5)) \
           == '00011H1111000'
