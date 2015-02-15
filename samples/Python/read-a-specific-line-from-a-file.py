from itertools import islice

with open('xxx.txt') as f:
    linelist = list(islice(f, 7, 8))
    assert linelist != [], 'Not 7 lines in file'
    line = linelist[0]
