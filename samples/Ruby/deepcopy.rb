# _orig_ is a Hash that contains an Array.
orig = { :num => 1, :ary => [2, 3] }
orig[:cycle] = orig	# _orig_ also contains itself.

# _copy_ becomes a deep copy of _orig_.
copy = Marshal.load(Marshal.dump orig)

# These changes to _orig_ never affect _copy_,
# because _orig_ and _copy_ are disjoint structures.
orig[:ary] << 4
orig[:rng] = (5..6)

# Because of deep copy, orig[:ary] and copy[:ary]
# refer to different Arrays.
p orig	# => {:num=>1, :ary=>[2, 3, 4], :cycle=>{...}, :rng=>5..6}
p copy	# => {:num=>1, :ary=>[2, 3], :cycle=>{...}}

# The original contains itself, and the copy contains itself,
# but the original and the copy are not the same object.
p [(orig.equal? orig[:cycle]),
   (copy.equal? copy[:cycle]),
   (not orig.equal? copy)]	# => [true, true, true]
