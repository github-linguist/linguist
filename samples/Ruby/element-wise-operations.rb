require 'matrix'

class Matrix
  def element_wise( operator, other )
    Matrix.build(row_size, column_size) do |row, col|
      self[row, col].send(operator, other[row, col])
    end
  end
end

m1, m2 = Matrix[[3,1,4],[1,5,9]], Matrix[[2,7,1],[8,2,2]]
puts "m1: #{m1}\nm2: #{m2}\n\n"

[:+, :-, :*, :/, :fdiv, :**, :%].each do |op|
  puts "m1 %-4s m2  =  %s" % [op, m1.element_wise(op, m2)]
end
