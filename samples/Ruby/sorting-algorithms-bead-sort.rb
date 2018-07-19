class Array
  def beadsort
    map {|e| [1] * e}.columns.columns.map {|e| e.length}
  end

  def columns
    y = length
    x = map {|l| l.length}.max
    Array.new(x) do |row|
      Array.new(y) { |column| self[column][row] }.compact # Remove nils.
    end
  end
end

# Demonstration code:
p [5,3,1,7,4,1,1].beadsort
