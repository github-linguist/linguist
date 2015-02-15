open("input_file","r") do f
    for line in eachline(f)
      println("read line: ", line)
    end
end
