method_names = [:==,:!=, :>, :>=, :<, :<=, :<=>, :casecmp]
[["YUP", "YUP"], ["YUP", "Yup"], ["bot","bat"], ["aaa", "zz"]].each do |str1, str2|
  method_names.each{|m| puts "%s %s %s\t%s" % [str1, m, str2, str1.send(m, str2)]}
  puts
end
