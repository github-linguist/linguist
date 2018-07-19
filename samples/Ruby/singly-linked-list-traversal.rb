head = ListNode.new("a", ListNode.new("b", ListNode.new("c")))
head.insertAfter("b", "b+")

# then:
head.each {|node| print node.value, ","}
puts

# or
current = head
begin
  print current.value, ","
end while current = current.succ
puts
