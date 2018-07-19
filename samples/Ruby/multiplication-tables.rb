def multiplication_table(n)
  puts "    " + ((" %3d" * n) % (1..n).to_a)
  1.upto(n) do |x|
    print "%3d " % x
    1.upto(x-1) {|y| print "    "}
    x.upto(n)   {|y| print " %3d" % (x*y)}
    puts ""
  end
end

multiplication_table 12
