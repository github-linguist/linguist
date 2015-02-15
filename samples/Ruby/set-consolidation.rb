require 'set'

tests = [[['A', 'B'], ['C','D']],
[['A','B'], ['B','D']],
[['A','B'], ['C','D'], ['D','B']],
[['H','I','K'], ['A','B'], ['C','D'], ['D','B'], ['F','G','H']]]
tests = tests.map{|sets| sets.map(&:to_set)}

tests.map do |sets|
  loop until sets.combination(2).none? do |a,b|
    if a.intersect?(b) then
      a.merge(b)
      sets.delete(b)
    end
  end
  p sets
end
