require 'thread'

nums = ARGV.collect(&:to_i)
sorted = []
mutex = Mutex.new

threads = nums.collect do |n|
  Thread.new do
    sleep 0.01 * n
    mutex.synchronize {sorted << n}
  end
end
threads.each {|t| t.join}

p sorted
