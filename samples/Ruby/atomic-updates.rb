require 'thread'

# A collection of buckets, filled with random non-negative integers.
# There are atomic operations to look at the bucket contents, and
# to move amounts between buckets.
class BucketStore

  # Creates a BucketStore with +nbuckets+ buckets. Fills each bucket
  # with a random non-negative integer.
  def initialize nbuckets
    # Create an array for the buckets
    @buckets = (0...nbuckets).map { rand(1024) }

    # Mutex used to make operations atomic
    @mutex = Mutex.new
  end

  # Returns an array with the contents of all buckets.
  def buckets
    @mutex.synchronize { Array.new(@buckets) }
  end

  # Transfers _amount_ to bucket at array index _destination_,
  # from bucket at array index _source_.
  def transfer destination, source, amount
    # Do nothing if both buckets are same
    return nil if destination == source

    @mutex.synchronize do
      # Clamp amount to prevent negative value in bucket
      amount = [amount, @buckets[source]].min

      @buckets[source] -= amount
      @buckets[destination] += amount
    end
    nil
  end
end

# Create bucket store
bucket_store = BucketStore.new 8

# Get total amount in the store
TOTAL = bucket_store.buckets.inject { |a, b| a += b }

# Start a thread to equalize buckets
Thread.new do
  loop do
    # Pick 2 buckets
    buckets = bucket_store.buckets
    first = rand buckets.length
    second = rand buckets.length

    # Swap buckets so that _first_ has not more than _second_
    first, second = second, first if buckets[first] > buckets[second]

    # Transfer half of the difference, rounded down
    bucket_store.transfer first, second, (buckets[second] - buckets[first]) / 2
  end
end

# Start a thread to distribute values among buckets
Thread.new do
  loop do
    # Pick 2 buckets
    buckets = bucket_store.buckets
    first = rand buckets.length
    second = rand buckets.length

    # Transfer random amount to _first_ from _second_
    bucket_store.transfer first, second, rand(buckets[second])
  end
end

# Loop to display buckets
loop do
  sleep 1

  buckets = bucket_store.buckets

  # Compute the total value in all buckets.
  # We calculate this outside BucketStore so BucketStore can't cheat by
  # always reporting the same value.
  n = buckets.inject { |a, b| a += b }

  # Display buckets and total
  printf "%s, total %d\n", (buckets.map { |v| sprintf "%4d", v }.join " "), n

  if n != TOTAL
    # This should never happen
    $stderr.puts "ERROR: Total changed from #{TOTAL} to #{n}"
    exit 1
  end
end
