class DListNode
  def insert_after(search_value, new_value)
    if search_value == value
      new_node = self.class.new(new_value, nil, nil)
      next_node = self.succ
      self.succ = new_node
      new_node.prev = self
      new_node.succ = next_node
      next_node.prev = new_node
    elsif self.succ.nil?
      raise StandardError, "value #{search_value} not found in list"
    else
      self.succ.insert_after(search_value, new_value)
    end
  end
end

head = DListNode.from_array([:a, :b])
head.insert_after(:a, :c)
