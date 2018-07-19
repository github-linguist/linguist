class BinaryTreeNode
  def initialize(value, left=nil, right=nil)
    @value, @left, @right = value, left, right
  end
  attr_reader :value, :left, :right

  def self.from_array(nested_list)
    value, left, right = nested_list
    if value
      self.new(value, self.from_array(left), self.from_array(right))
    end
  end

  def walk_nodes(order, &block)
    order.each do |node|
      case node
      when :left  then left && left.walk_nodes(order, &block)
      when :self  then yield self
      when :right then right && right.walk_nodes(order, &block)
      end
    end
  end

  def each_preorder(&b)  ; walk_nodes([:self, :left, :right], &b) ; end
  def each_inorder(&b)   ; walk_nodes([:left, :self, :right], &b) ; end
  def each_postorder(&b) ; walk_nodes([:left, :right, :self], &b) ; end

  def each_levelorder
    queue = [self]
    until queue.empty?
      node = queue.shift
      yield node
      queue << node.left if node.left
      queue << node.right if node.right
    end
  end
end

root = BinaryTreeNode.from_array [1, [2, [4, 7], [5]], [3, [6, [8], [9]]]]

%w{each_preorder each_inorder each_postorder each_levelorder}.each {|mthd|
  printf "%-11s ", mthd[5..-1] + ':'
  root.send(mthd) {|node| print "#{node.value} "}
  puts
}
