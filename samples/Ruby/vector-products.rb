require 'matrix'

class Vector
  def cross_product(v)
    unless size == 3 && v.size == 3
      raise ArgumentError, "Vectors must have size 3"
    end
    Vector[self[1] * v[2] - self[2] * v[1],
           self[2] * v[0] - self[0] * v[2],
           self[0] * v[1] - self[1] * v[0]]
  end

  def scalar_triple_product(b, c)
    self.inner_product(b.cross_product c)
  end

  def vector_triple_product(b, c)
    self.cross_product(b.cross_product c)
  end
end

a = Vector[3, 4, 5]
b = Vector[4, 3, 5]
c = Vector[-5, -12, -13]

puts "a dot b = #{a.inner_product b}"
puts "a cross b = #{a.cross_product b}"
puts "a dot (b cross c) = #{a.scalar_triple_product b, c}"
puts "a cross (b cross c) = #{a.vector_triple_product b, c}"
