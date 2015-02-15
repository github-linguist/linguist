require 'socket'

# A Workshop runs all of its workers, then collects their results. Use
# Workshop#add to add workers and Workshop#work to run them.
#
# This implementation forks some processes to run the workers in
# parallel. Ruby must provide Kernel#fork and 'socket' library must
# provide UNIXSocket.
#
# Why processes and not threads? C Ruby still has a Global VM Lock,
# where only one thread can hold the lock. One platform, OpenBSD, still
# has userspace threads, with all threads on one cpu core. Multiple
# processes will not compete for a single Global VM Lock and can run
# on multiple cpu cores.
class Workshop
  # Creates a Workshop.
  def initialize
    @sockets = {}
  end

  # Adds a worker to this Workshop. Returns a worker id _wid_ for this
  # worker. The worker is a block that takes some _args_ and returns
  # some value. Workshop#work will run the block.
  #
  # This implementation forks a process for the worker. This process
  # will use Marshal with UNIXSocket to receive the _args_ and to send
  # the return value. The _wid_ is a process id. The worker also
  # inherits _IO_ objects, which might be a problem if the worker holds
  # open a pipe or socket, and the other end never reads EOF.
  def add
    child, parent = UNIXSocket.pair

    wid = fork do
      # I am the child.
      child.close
      @sockets.each_value { |sibling| sibling.close }

      # Prevent that all the children print their backtraces (to a mess
      # of mixed lines) when user presses Control-C.
      Signal.trap("INT") { exit! }

      loop do
        # Wait for a command.
        begin
          command, args = Marshal.load(parent)
        rescue EOFError
          # Parent probably died.
          break
        end

        case command
        when :work
          # Do work. Send result to parent.
          result = yield *args
          Marshal.dump(result, parent)
        when :remove
          break
        else
          fail "bad command from workshop"
        end
      end
    end

    # I am the parent.
    parent.close
    @sockets[wid] = child
    wid
  end

  # Runs all of the workers, and collects the results in a Hash. Passes
  # the same _args_ to each of the workers. Returns a Hash that pairs
  # _wid_ => _result_, where _wid_ is the worker id and _result_ is the
  # return value from the worker.
  #
  # This implementation runs the workers in parallel, and waits until
  # _all_ of the workers finish their results. Workshop provides no way
  # to start the work without waiting for the work to finish. If a
  # worker dies (for example, by raising an Exception), then
  # Workshop#work raises a RuntimeError.
  def work(*args)
    message = [:work, args]
    @sockets.each_pair do |wid, child|
      Marshal.dump(message, child)
    end

    # Checkpoint! Wait for all workers to finish.
    result = {}
    @sockets.each_pair do |wid, child|
      begin
        # This waits until the child finishes a result.
        result[wid] = Marshal.load(child)
      rescue EOFError
        fail "Worker #{wid} died"
      end
    end
    result
  end

  # Removes a worker from the Workshop, who has a worker id _wid_.
  # If there is no such worker, raises ArgumentError.
  #
  # This implementation kills and reaps the process for the worker.
  def remove(wid)
    unless child = @sockets.delete(wid)
      raise ArgumentError, "No worker #{wid}"
    else
      Marshal.dump([:remove, nil], child)
      child.close
      Process.wait(wid)
    end
  end
end



# First create a Workshop.
require 'pp'
shop = Workshop.new
wids = []

# Our workers must not use the same random numbers after the fork.
@fixed_rand = false
def fix_rand
  unless @fixed_rand; srand; @fixed_rand = true; end
end

# Start with some workers.
6.times do
  wids << shop.add do |i|
    # This worker slowly calculates a Fibonacci number.
    fix_rand
    f = proc { |n| if n < 2 then n else f[n - 1] + f[n - 2] end }
    [i, f[25 + rand(10)]]
  end
end

6.times do |i|
  # Do one cycle of work, and print the result.
  pp shop.work(i)

  # Remove a worker.
  victim = rand(wids.length)
  shop.remove wids[victim]
  wids.slice! victim

  # Add another worker.
  wids << shop.add do |j|
    # This worker slowly calculates a number from
    # the sequence 0, 1, 2, 3, 6, 11, 20, 37, 68, 125, ...
    fix_rand
    f = proc { |n| if n < 3 then n else f[n - 1] + f[n - 2] + f[n - 3] end }
    [j, i, f[20 + rand(10)]]
  end
end

# Remove all workers.
wids.each { |wid| shop.remove wid }
pp shop.work(6)
