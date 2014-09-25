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

# This module beef up the clock module by allowing a clock to be comparable.
# It show the usage of class refinement
module clock_more

import clock

redef class Clock
	# Clock are now comparable
	super Comparable

	# Comparaison of a clock make only sense with an other clock
	redef type OTHER: Clock

	redef fun <(o)
	do
		# Note: < is the only abstract method of Comparable.
		#       All other operators and methods rely on < and ==.
		return self.total_minutes < o.total_minutes
	end
end

var c1 = new Clock(8, 12)
var c2 = new Clock(8, 13)
var c3 = new Clock(9, 13)

print "{c1}<{c2}? {c1<c2}"
print "{c1}<={c2}? {c1<=c2}"
print "{c1}>{c2}? {c1>c2}"
print "{c1}>={c2}? {c1>=c2}"
print "{c1}<=>{c2}? {c1<=>c2}"
print "{c1},{c2}? max={c1.max(c2)} min={c1.min(c2)}"
print "{c1}.is_between({c2}, {c3})? {c1.is_between(c2, c3)}"
print "{c2}.is_between({c1}, {c3})? {c2.is_between(c1, c3)}"

print "-"

c1.minutes += 1

print "{c1}<{c2}? {c1<c2}"
print "{c1}<={c2}? {c1<=c2}"
print "{c1}>{c2}? {c1>c2}"
print "{c1}>={c2}? {c1>=c2}"
print "{c1}<=>{c2}? {c1<=>c2}"
print "{c1},{c2}? max={c1.max(c2)} min={c1.min(c2)}"
print "{c1}.is_between({c2}, {c3})? {c1.is_between(c2, c3)}"
print "{c2}.is_between({c1}, {c3})? {c2.is_between(c1, c3)}"
