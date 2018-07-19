def bubble_sort(seq):
    """Inefficiently sort the mutable sequence (list) in place.
       seq MUST BE A MUTABLE SEQUENCE.

       As with list.sort() and random.shuffle this does NOT return
    """
    changed = True
    while changed:
        changed = False
        for i in xrange(len(seq) - 1):
            if seq[i] > seq[i+1]:
                seq[i], seq[i+1] = seq[i+1], seq[i]
                changed = True
    return None

if __name__ == "__main__":
   """Sample usage and simple test suite"""

   from random import shuffle

   testset = range(100)
   testcase = testset[:] # make a copy
   shuffle(testcase)
   assert testcase != testset  # we've shuffled it
   bubble_sort(testcase)
   assert testcase == testset  # we've unshuffled it back into a copy
