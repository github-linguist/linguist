class DListNode
  def get_tail
    # parent class (ListNode) includes Enumerable, so the find method is available to us
    self.find {|node| node.succ.nil?}
  end

  def each_previous(&b)
    yield self
    self.prev.each_previous(&b) if self.prev
  end
end

head = DListNode.from_array([:a, :b, :c])
head.each {|node| p node.value}
head.get_tail.each_previous {|node| p node.value}
