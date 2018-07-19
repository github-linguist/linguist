def compose(f,g)
  lambda {|x| f[g[x]]}
end
s = compose(Math.method(:sin), Math.method(:cos))
p s[0.5]  # => 0.769196354841008

# verify
p Math.sin(Math.cos(0.5))  # => 0.769196354841008
