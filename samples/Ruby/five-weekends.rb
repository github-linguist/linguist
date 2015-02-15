require 'date'

# if the last day of the month falls on a Sunday and the month has 31 days,
# this is the only case where the month has 5 weekends.
start = Date.parse("1900-01-01")
stop  = Date.parse("2100-12-31")
dates = (start..stop).find_all do |day|
  day.mday == 31 and day.wday == 0 # Ruby 1.9: and day.sunday?
end

puts "There are #{dates.size} months with 5 weekends from 1900 to 2100:"
puts dates.first(5).map { |d| d.strftime("%b %Y") }
puts "..."
puts dates.last(5).map { |d| d.strftime("%b %Y") }

years_with_5w = dates.map(&:year)

years = (1900..2100).to_a - years_with_5w

puts "There are #{years.size} years without months with 5 weekends:"
puts years.join(", ")
