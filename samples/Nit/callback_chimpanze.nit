# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Matthieu Lucas <lucasmatthieu@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This sample has been implemented to show you how simple is it to play 
# with native callbacks (C) through an high level with NIT program.

module callback_chimpanze
import callback_monkey

class Chimpanze
	super MonkeyActionCallable

	fun create
	do
		var monkey = new Monkey
		print "Hum, I'm sleeping ..."
		# Invoking method which will take some time to compute, and 
		# will be back in wokeUp method with information.
		# - Callback method defined in MonkeyActionCallable Interface
		monkey.wokeUpAction(self, "Hey, I'm awake.")
	end

	# Inherit callback method, defined by MonkeyActionCallable interface
	# - Back of wokeUpAction method 
	redef fun wokeUp( sender:Monkey, message:Object )
	do
		print message
	end
end

var m = new Chimpanze
m.create
