inf = 1.0 / 0.0    #=> Infinity
nan = 0.0 / 0.0    #=> NaN

expression = [
  "1.0 / 0.0", "-1.0 / 0.0", "0.0 / 0.0", "- 0.0",
  "inf + 1", "5 - inf", "inf * 5", "inf / 5", "inf * 0",
  "1.0 / inf", "-1.0 / inf", "inf + inf", "inf - inf",
  "inf * inf", "inf / inf", "inf * 0.0", " 0 < inf", "inf == inf",
  "nan + 1", "nan * 5", "nan - nan", "nan * inf", "- nan",
  "nan == nan", "nan > 0", "nan < 0", "nan == 0", "nan <=> 0.0", "0.0 == -0.0",
]

expression.each do |exp|
  puts "%15s => %p" % [exp, eval(exp)]
end
