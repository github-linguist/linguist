# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This module provide a simple wall clock.
# It is an example of getters and setters.
# A beefed-up module is available in clock_more
module clock

# A simple wall clock with 60 minutes and 12 hours.
class Clock
	# total number of minutes from 0 to 719
	var total_minutes: Int
	# Note: only the read acces is public, the write access is private.

	# number of minutes in the current hour (from 0 to 59)
	fun minutes: Int do return self.total_minutes % 60
	
	# set the number of minutes in the current hour.
	# if m < 0 or m >= 60, the hour will be changed accordinlgy
	fun minutes=(m: Int) do self.total_minutes = self.hours * 60 + m

	# number of hours (from 0 to 11)
	fun hours: Int do return self.total_minutes / 60

	# set the number of hours
	# the minutes will not be updated
	fun hours=(h: Int) do self.total_minutes = h * 60 + minutes

	# the position of the hour arrow in the [0..60[ interval
	fun hour_pos: Int do return total_minutes / 12

	# replace the arrow of hours (from 0 to 59).
	# the hours and the minutes will be updated.
	fun hour_pos=(h: Int) do self.total_minutes = h * 12

	redef fun to_s do return "{hours}:{minutes}"

	fun reset(hours, minutes: Int) do self.total_minutes = hours*60 + minutes

	init(hours, minutes: Int) do self.reset(hours, minutes)

	redef fun ==(o)
	do
		# Note: o is a nullable Object, a type test is required
		# Thanks to adaptive typing, there is no downcast
		# i.e. the code is safe!
		return o isa Clock and self.total_minutes == o.total_minutes
	end
end

var c = new Clock(10,50)
print "It's {c} o'clock."

c.minutes += 22
print "Now it's {c} o'clock."

print "The short arrow in on the {c.hour_pos/5} and the long arrow in on the {c.minutes/5}."

c.hours -= 2
print "Now it's {c} o'clock."

var c2 = new Clock(9, 11)
print "It's {c2} on the second clock."
print "The two clocks are synchronized: {c == c2}."
c2.minutes += 1
print "It's now {c2} on the second clock."
print "The two clocks are synchronized: {c == c2}."
