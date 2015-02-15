# Returns a random line from _io_, or nil if _io_ has no lines.
#  # Get a random line from /etc/passwd
#  line = open("/etc/passwd") {|f| random_line(f) }
def random_line(io)
  choice = io.gets; count = 1
  while line = io.gets
    rand(count += 1).zero? and choice = line
  end
  choice
end

def one_of_n(n)
  # Create a mock IO that provides line numbers instead of lines.
  # Assumes that #random_line calls #gets.
  (mock_io = Object.new).instance_eval do
    @count = 0
    @last = n
    def self.gets
      (@count < @last) ? (@count += 1) : nil
    end
  end
  random_line(mock_io)
end

chosen = Hash.new(0)
1_000_000.times { chosen[one_of_n(10)] += 1 }
chosen.keys.sort.each do |key|
  puts "#{key} chosen #{chosen[key]} times"
end
