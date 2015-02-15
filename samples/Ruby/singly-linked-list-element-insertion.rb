class ListNode
  def insert_after(search_value, new_value)
    if search_value == value
      self.succ = self.class.new(new_value, succ)
    elsif self.succ.nil?
      raise StandardError, "value #{search_value} not found in list"
    else
      self.succ.insert_after(search_value, new_value)
    end
  end
end

list = ListNode.new(:a, ListNode.new(:b))
list.insert_after(:a, :c)
