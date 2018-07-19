require './distcheck.rb'

def d5
  1 + rand(5)
end

def d7
  loop do
    d55 = 5*d5 + d5 - 6
    return (d55 % 7 + 1) if d55 < 21
  end
end

distcheck(1_000_000) {d5}
distcheck(1_000_000) {d7}
