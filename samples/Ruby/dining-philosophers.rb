require 'mutex_m'

class Philosopher
  def initialize(name, left_fork, right_fork)
    @name = name
    @left_fork = left_fork
    @right_fork = right_fork
    @meals = 0
  end

  def go
    while @meals < 5
      think
      dine
    end
    puts "philosopher #@name is full!"
  end

  def think
    puts "philosopher #@name is thinking..."
    sleep(rand())
    puts "philosopher #@name is hungry..."
  end

  def dine
    fork1, fork2 = @left_fork, @right_fork
    while true
      pickup(fork1, :wait => true)
      puts "philosopher #@name has fork #{fork1.fork_id}..."
      if pickup(fork2, :wait => false)
        break
      end
      puts "philosopher #@name cannot pickup second fork #{fork2.fork_id}..."
      release(fork1)
      fork1, fork2 = fork2, fork1
    end
    puts "philosopher #@name has the second fork #{fork2.fork_id}..."

    puts "philosopher #@name eats..."
    sleep(rand())
    puts "philosopher #@name belches"
    @meals += 1

    release(@left_fork)
    release(@right_fork)
  end

  def pickup(fork, opt)
    puts "philosopher #@name attempts to pickup fork #{fork.fork_id}..."
    opt[:wait] ? fork.mutex.mu_lock : fork.mutex.mu_try_lock
  end

  def release(fork)
    puts "philosopher #@name releases fork #{fork.fork_id}..."
    fork.mutex.unlock
  end
end

n = 5

Fork = Struct.new(:fork_id, :mutex)
forks = Array.new(n) {|i| Fork.new(i, Object.new.extend(Mutex_m))}

philosophers = Array.new(n) do |i|
                 Thread.new(i, forks[i], forks[(i+1)%n]) do |id, f1, f2|
                   ph = Philosopher.new(id, f1, f2).go
                 end
               end

philosophers.each {|thread| thread.join}
