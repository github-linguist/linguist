re = /\A     # beginning of string
  (?<bb>     # begin capture group <bb>
    \[       #   literal [
    \g<bb>*  #   zero or more <bb>
    \]       #   literal ]
  )*         # end group, zero or more such groups
\z/x         # end of string

10.times do |i|
  s = (%w{[ ]} * i).shuffle.join
  puts (s =~ re ? " OK: " : "bad: ") + s
end

["[[]", "[]]", "a[ letters[-1] ].xyz[0]"].each do |s|
  t = s.gsub(/[^\[\]]/, "")
  puts (t =~ re ? " OK: " : "bad: ") + s
end
