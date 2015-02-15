#encoding: UTF-8       # superfluous in Ruby >1.9.3

if ENV.values_at("LC_ALL","LC_CTYPE","LANG").compact.first.include?("UTF-8")
  puts "△"
else
  raise "Terminal can't handle UTF-8"
end
