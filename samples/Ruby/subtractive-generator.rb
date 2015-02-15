# SubRandom is a subtractive random number generator which generates
# the same sequences as Bentley's generator, as used in xpat2.
class SubRandom
  # The original seed of this generator.
  attr_reader :seed

  # Creates a SubRandom generator with the given _seed_.
  # The _seed_ must be an integer from 0 to 999_999_999.
  def initialize(seed = Kernel.rand(1_000_000_000))
    (0..999_999_999).include? seed or
      raise ArgumentError, "seed not in 0..999_999_999"

    # @state = 55 elements.
    ary = [seed, 1]
    53.times { ary << ary[-2] - ary[-1] }
    @state = []
    34.step(1870, 34) {|i| @state << ary[i % 55] }

    220.times { rand }  # Discard first 220 elements of sequence.

    @seed = seed        # Save original seed.
  end

  # Duplicates internal state so SubRandom#dup never shares state.
  def initialize_copy(orig)
    @state = @state.dup
  end

  # Returns the next random integer, from 0 to 999_999_999.
  def rand
    @state << (@state[-55] - @state[-24]) % 1_000_000_000
    @state.shift
  end
end

rng = SubRandom.new(292929)
p (1..3).map { rng.rand }
