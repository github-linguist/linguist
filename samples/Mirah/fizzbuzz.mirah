1.upto(100) do |n|
    print "Fizz" if a = ((n % 3) == 0)
    print "Buzz" if b = ((n % 5) == 0)
    print n unless (a || b)
    print "\n"
end

# a little more straight forward
1.upto(100) do |n|
    if (n % 15) == 0
        puts "FizzBuzz"
    elsif (n % 5) == 0
        puts "Buzz"
    elsif (n % 3) == 0
        puts "Fizz"
    else
        puts n
    end
end
