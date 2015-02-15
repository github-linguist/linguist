#!/usr/bin/env python

import time

print "\033[?1049h\033[H"
print "Alternate buffer!"

for i in xrange(5, 0, -1):
    print "Going back in:", i
    time.sleep(1)

print "\033[?1049l"
