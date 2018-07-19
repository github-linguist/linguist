#!/usr/bin/env python

import fileinput, sys

fname, start, count = sys.argv[1:4]
start, count = int(start), int(count)

for line in fileinput.input(fname, inplace=1, backup='.orig'):
    if start <= fileinput.lineno() < start + count:
        pass
    else:
        print line[:-1]
fileinput.close()
