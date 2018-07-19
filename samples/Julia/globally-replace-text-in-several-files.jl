filenames = ["f1.txt", "f2.txt"]
for filename in filenames
   txt = readall(filename)
   open(filename, "w") do f
      write(f, replace(txt, "Goodbye London!", "Hello New York!"))
   end
end
