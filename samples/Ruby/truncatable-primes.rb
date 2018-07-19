def left_truncatable?(n)
  truncatable?(n) {|i| i.to_s[1..-1].to_i}
end


def right_truncatable?(n)
  truncatable?(n) {|i| i/10}
end

def truncatable?(n, &trunc_func)
  return false if n.to_s.include? "0"
  loop do
    n = trunc_func.call(n)
    return true if n.zero?
    return false unless Prime.prime?(n)
  end
end

require 'prime'
primes = Prime.each(1_000_000).to_a.reverse

p primes.detect {|p| left_truncatable? p}
p primes.detect {|p| right_truncatable? p}
