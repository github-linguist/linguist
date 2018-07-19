class TwentyFourGamePlayer
  EXPRESSIONS = [
    '((%d %s %d) %s %d) %s %d',
    '(%d %s (%d %s %d)) %s %d',
    '(%d %s %d) %s (%d %s %d)',
    '%d %s ((%d %s %d) %s %d)',
    '%d %s (%d %s (%d %s %d))',
  ].map{|expr| [expr, expr.gsub('%d', 'Rational(%d,1)')]}

  OPERATORS = [:+, :-, :*, :/].repeated_permutation(3)

  OBJECTIVE = Rational(24,1)

  def self.solve(digits)
    solutions = []
    digits.permutation.to_a.uniq.each do |a,b,c,d|
      OPERATORS.each do |op1,op2,op3|
        EXPRESSIONS.each do |expr,expr_rat|
          # evaluate using rational arithmetic
          test = expr_rat % [a, op1, b, op2, c, op3, d]
          value = eval(test) rescue -1  # catch division by zero
          if value == OBJECTIVE
            solutions << expr % [a, op1, b, op2, c, op3, d]
          end
        end
      end
    end
    solutions
  end
end

# validate user input
digits = ARGV.map do |arg|
  begin
    Integer(arg)
  rescue ArgumentError
    raise "error: not an integer: '#{arg}'"
  end
end
digits.size == 4 or raise "error: need 4 digits, only have #{digits.size}"

solutions = TwentyFourGamePlayer.solve(digits)
if solutions.empty?
  puts "no solutions"
else
  puts "found #{solutions.size} solutions, including #{solutions.first}"
  puts solutions.sort
end
