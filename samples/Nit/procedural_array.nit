# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2004-2008 Jean Privat <jean@pryen.org>
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

# A procedural program (without explicit class definition).
# This program manipulates arrays of integers.
module procedural_array

# The sum of the elements of `a'.
# Uses a 'for' control structure.
fun array_sum(a: Array[Int]): Int
do
	var sum = 0
	for i in a do
		sum = sum + i
	end
	return sum
end

# The sum of the elements of `a' (alternative version).
# Uses a 'while' control structure.
fun array_sum_alt(a: Array[Int]): Int
do
	var sum = 0
	var i = 0
	while i < a.length do
		sum = sum + a[i]
		i = i + 1
	end
	return sum
end

# The main part of the program.
var a = [10, 5, 8, 9]
print(array_sum(a))
print(array_sum_alt(a))
