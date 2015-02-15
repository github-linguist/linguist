class PriorityQueueNaive
  def initialize
    @q = Hash.new { |h, k| h[k] = []}
    @priorities = []
  end

  def push(priority, item)
    @q[priority] << item
    @priorities = @q.keys.sort
  end

  def pop
    p = @priorities[0]
    item = @q[p].shift
    if @q[p].empty?
      @q.delete(p)
      @priorities.shift
    end
    item
  end

  def peek
    if not empty?
      @q[@priorities[0]][0]
    end
  end

  def empty?
    @priorities.empty?
  end

  def inspect
    @q.inspect
  end
end

test = [
  [6, "drink tea"],
  [3, "Clear drains"],
  [4, "Feed cat"],
  [5, "Make tea"],
  [6, "eat biscuit"],
  [1, "Solve RC tasks"],
  [2, "Tax return"],
]

pq = PriorityQueueNaive.new
test.each {|pr, str| pq.push(pr, str) }
until pq.empty?
  puts pq.pop
end
