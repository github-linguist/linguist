class DListNode < ListNode
  attr_accessor :prev
  # accessors :succ and :value are inherited

  def initialize(value, prev=nil, succ=nil)
    @value = value
    @prev = prev
    @prev.succ = self if prev
    @succ = succ
    @succ.prev = self if succ
  end

  def self.from_values(*ary)
    ary << (f = ary.pop)
    ary.map! {|i| new i }
    ary.inject(f) {|p, c| p.succ = c; c.prev = p; c }
  end
end

list = DListNode.from_values 1,2,3,4
