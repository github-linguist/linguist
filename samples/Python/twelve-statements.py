from itertools import product
#from pprint import pprint as pp

constraintinfo = (
  (lambda st: len(st) == 12                 ,(1, 'This is a numbered list of twelve statements')),
  (lambda st: sum(st[-6:]) == 3             ,(2, 'Exactly 3 of the last 6 statements are true')),
  (lambda st: sum(st[1::2]) == 2            ,(3, 'Exactly 2 of the even-numbered statements are true')),
  (lambda st: (st[5]&st[6]) if st[4] else 1 ,(4, 'If statement 5 is true, then statements 6 and 7 are both true')),
  (lambda st: sum(st[1:4]) == 0             ,(5, 'The 3 preceding statements are all false')),
  (lambda st: sum(st[0::2]) == 4            ,(6, 'Exactly 4 of the odd-numbered statements are true')),
  (lambda st: sum(st[1:3]) == 1             ,(7, 'Either statement 2 or 3 is true, but not both')),
  (lambda st: (st[4]&st[5]) if st[6] else 1 ,(8, 'If statement 7 is true, then 5 and 6 are both true')),
  (lambda st: sum(st[:6]) == 3              ,(9, 'Exactly 3 of the first 6 statements are true')),
  (lambda st: (st[10]&st[11])               ,(10, 'The next two statements are both true')),
  (lambda st: sum(st[6:9]) == 1             ,(11, 'Exactly 1 of statements 7, 8 and 9 are true')),
  (lambda st: sum(st[0:11]) == 4            ,(12, 'Exactly 4 of the preceding statements are true')),
)

def printer(st, matches):
    if False in matches:
        print('Missed by one statement: %i, %s' % docs[matches.index(False)])
    else:
        print('Full match:')
    print('  ' + ', '.join('%i:%s' % (i, 'T' if t else 'F') for i, t in enumerate(st, 1)))

funcs, docs = zip(*constraintinfo)

full, partial = [], []

for st in product( *([(False, True)] * 12) ):
    truths = [bool(func(st)) for func in funcs]
    matches = [s == t for s,t in zip(st, truths)]
    mcount = sum(matches)
    if mcount == 12:
        full.append((st, matches))
    elif mcount == 11:
        partial.append((st, matches))

for stm in full + partial:
    printer(*stm)
