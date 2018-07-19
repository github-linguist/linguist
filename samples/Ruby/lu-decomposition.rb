require 'matrix'

class Matrix
  def lu_decomposition
    p = get_pivot
    tmp = p * self
    u = Matrix.zero(row_size).to_a
    l = Matrix.identity(row_size).to_a
    (0 ... row_size).each do |i|
      (0 ... row_size).each do |j|
        if j >= i
          # upper
          u[i][j] = tmp[i,j] - (0 .. i-1).inject(0.0) {|sum, k| sum + u[k][j] * l[i][k]}
        else
          # lower
          l[i][j] = (tmp[i,j] - (0 .. j-1).inject(0.0) {|sum, k| sum + u[k][j] * l[i][k]}) / u[j][j]
        end
      end
    end
    [ Matrix[*l], Matrix[*u], p ]
  end

  def get_pivot
    raise ArgumentError, "must be square" unless square?
    id = Matrix.identity(row_size).to_a
    (0 ... row_size).each do |i|
      max = self[i,i]
      row = i
      (i ... row_size).each do |j|
        if self[j,i] > max
          max = self[j,i]
          row = j
        end
      end
      id[i], id[row] = id[row], id[i]
    end
    Matrix[*id]
  end

  def pretty_print(format)
    each_with_index do |val, i, j|
      print "#{format} " % val
      puts "" if j==column_size-1
    end
  end
end

a = Matrix[[1,  3,  5],
           [2,  4,  7],
           [1,  1,  0]]
puts "A"; a.pretty_print("%2d")
l, u, p = a.lu_decomposition
puts "U"; u.pretty_print("%8.5f")
puts "L"; l.pretty_print("%8.5f")
puts "P"; p.pretty_print("%d")

a = Matrix[[11, 9,24,2],
           [ 1, 5, 2,6],
           [ 3,17,18,1],
           [ 2, 5, 7,1]]
puts "A"; a.pretty_print("%2d")
l, u, p = a.lu_decomposition
puts "U"; u.pretty_print("%8.5f")
puts "L"; l.pretty_print("%8.5f")
puts "P"; p.pretty_print("%d")
