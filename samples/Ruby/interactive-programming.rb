$ irb
irb(main):001:0> def f(string1, string2, separator)
irb(main):002:1>     [string1, '', string2].join(separator)
irb(main):003:1> end
=> nil
irb(main):004:0> f('Rosetta', 'Code', ':')
=> "Rosetta::Code"
irb(main):005:0> exit
$
