class Array
  def permutationsort
    permutation.each{|perm| return perm if perm.sorted?}
  end

  def sorted?
    each_cons(2).all? {|a, b| a <= b}
  end
end
