puts 'Enter width and height: '
w=gets.to_i
arr = Array.new(gets.to_i){Array.new(w)}
arr[1][3] = 5
p arr[1][3]
