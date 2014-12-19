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

# A simple exemple of refinement where a method is added to the integer class.
module fibonacci

redef class Int
	# Calculate the self-th element of the fibonacci sequence.
	fun fibonacci: Int
	do
		if self < 2 then
			return 1
		else
			return (self-2).fibonacci + (self-1).fibonacci
		end
	end 
end

# Print usage and exit.
fun usage
do
	print "Usage: fibonnaci <integer>" 
	exit 0 
end

# Main part
if args.length != 1 then
	usage
end
print args.first.to_i.fibonacci
