# 1. Divide n by 12. Remember the remainder (n is 8 for the eight queens
#    puzzle).
# 2. Write a list of the even numbers from 2 to n in order.
# 3. If the remainder is 3 or 9, move 2 to the end of the list.
# 4. Append the odd numbers from 1 to n in order, but, if the remainder is 8,
#    switch pairs (i.e. 3, 1, 7, 5, 11, 9, …).
# 5. If the remainder is 2, switch the places of 1 and 3, then move 5 to the
#    end of the list.
# 6. If the remainder is 3 or 9, move 1 and 3 to the end of the list.
# 7. Place the first-column queen in the row with the first number in the
#    list, place the second-column queen in the row with the second number in
#    the list, etc.

def n_queens(n)
  if n == 1
    return "Q"
  elsif n < 4
    puts "no solutions for n=#{n}"
    return ""
  end

  evens = (2..n).step(2).to_a
  odds = (1..n).step(2).to_a

  rem = n % 12  # (1)
  nums = evens  # (2)

  nums.push(nums.shift) if rem == 3 or rem == 9  # (3)

  # (4)
  if rem == 8
    odds = odds.each_slice(2).inject([]) {|ary, (a,b)| ary += [b,a]}
  end
  nums.concat(odds)

  # (5)
  if rem == 2
    idx = []
    [1,3,5].each {|i| idx[i] = nums.index(i)}
    nums[idx[1]], nums[idx[3]] = nums[idx[3]], nums[idx[1]]
    nums.slice!(idx[5])
    nums.push(5)
  end

  # (6)
  if rem == 3 or rem == 9
    [1,3].each do |i|
      nums.slice!( nums.index(i) )
      nums.push(i)
    end
  end

  # (7)
  board = Array.new(n) {Array.new(n) {"."}}
  n.times {|i| board[i][nums[i] - 1] = "Q"}
  board.inject("") {|str, row| str << row.join(" ") << "\n"}
end

(1 .. 15).each {|n| puts "n=#{n}"; puts n_queens(n); puts}
