class ListNode
  attr_accessor :value, :succ

  def initialize(value, succ=nil)
    self.value = value
    self.succ = succ
  end

  def each(&b)
    yield self
    succ.each(&b) if succ
  end

  include Enumerable

  def self.from_array(ary)
    head = self.new(ary[0], nil)
    prev = head
    ary[1..-1].each do |val|
      node = self.new(val, nil)
      prev.succ = node
      prev = node
    end
    head
  end
end

list = ListNode.from_array([1,2,3,4])
