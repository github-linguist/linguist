require 'date'

# Creates a calendar of _year_. Returns this calendar as a multi-line
# string fit to _columns_.
def cal(year, columns)

  # Start at January 1.
  #
  # Date::ENGLAND marks the switch from Julian calendar to Gregorian
  # calendar at 1752 September 14. This removes September 3 to 13 from
  # year 1752. (By fortune, it keeps January 1.)
  #
  date = Date.new(year, 1, 1, Date::ENGLAND)

  # Collect calendars of all 12 months.
  months = (1..12).collect do |month|
    rows = [Date::MONTHNAMES[month].center(20), "Su Mo Tu We Th Fr Sa"]

    # Make array of 42 days, starting with Sunday.
    days = []
    date.wday.times { days.push "  " }
    while date.month == month
      days.push("%2d" % date.mday)
      date += 1
    end
    (42 - days.length).times { days.push "  " }

    days.each_slice(7) { |week| rows.push(week.join " ") }
    next rows
  end

  # Calculate months per row (mpr).
  #  1. Divide columns by 22 columns per month, rounded down. (Pretend
  #     to have 2 extra columns; last month uses only 20 columns.)
  #  2. Decrease mpr if 12 months would fit in the same months per
  #     column (mpc). For example, if we can fit 5 mpr and 3 mpc, then
  #     we use 4 mpr and 3 mpc.
  mpr = (columns + 2).div 22
  mpr = 12.div((12 + mpr - 1).div mpr)

  # Use 20 columns per month + 2 spaces between months.
  width = mpr * 22 - 2

  # Join months into calendar.
  rows = ["[Snoopy]".center(width), "#{year}".center(width)]
  months.each_slice(mpr) do |slice|
    slice[0].each_index do |i|
      rows.push(slice.map {|a| a[i]}.join "  ")
    end
  end
  return rows.join("\n")
end


ARGV.length == 1 or abort "usage: #{$0} year"

# Guess width of terminal.
#  1. Obey environment variable COLUMNS.
#  2. Try to require 'io/console' from Ruby 1.9.3.
#  3. Try to run `tput co`.
#  4. Assume 80 columns.
columns = begin Integer(ENV["COLUMNS"] || "")
          rescue
            begin require 'io/console'; IO.console.winsize[1]
            rescue LoadError
              begin Integer(`tput co`)
              rescue
                80; end; end; end

puts cal(Integer(ARGV[0]), columns)
