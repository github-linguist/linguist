require 'abstraction'

class AbstractQueue
  abstract
  def enqueue(object)
    raise NotImplementedError
  end
  def dequeue
    raise NotImplementedError
  end
end

class ConcreteQueue < AbstractQueue
  def enqueue(object)
    puts "enqueue #{object.inspect}"
  end
end
