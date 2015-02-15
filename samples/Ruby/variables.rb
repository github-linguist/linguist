$a_global_var = 5
class Demo
  @@a_class_var = 6
  A_CONSTANT = 8
  def initialize
    @an_instance_var = 7
  end
  def incr(a_local_var)
    @an_instance_var += a_local_var
  end
end
