@target = "METHINKS IT IS LIKE A WEASEL"
Charset = [" ", *"A".."Z"]
COPIES = 100

def random_char; Charset.sample end

def fitness(candidate)
  sum = 0
  candidate.chars.zip(@target.chars) {|x,y| sum += (x[0].ord - y[0].ord).abs}
  100.0 * Math.exp(Float(sum) / -10.0)
end

def mutation_rate(candidate)
  1.0 - Math.exp( -(100.0 - fitness(candidate)) / 400.0)
end

def mutate(parent, rate)
  parent.each_char.collect {|ch| rand <= rate ? random_char : ch}.join
end

def log(iteration, rate, parent)
  puts "%4d %.2f %5.1f %s" % [iteration, rate, fitness(parent), parent]
end

iteration = 0
parent = Array.new(@target.length) {random_char}.join
prev = ""

while parent != @target
  iteration += 1
  rate = mutation_rate(parent)
  if prev != parent
    log(iteration, rate, parent)
    prev = parent
  end
  copies = [parent] + Array.new(COPIES) {mutate(parent, rate)}
  parent = copies.max_by {|c| fitness(c)}
end
log(iteration, rate, parent)
