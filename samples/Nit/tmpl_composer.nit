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

import template

### Here, definition of the specific templates

# The root template for composers
class TmplComposers
	super Template

	# Short list of composers
	var composers = new Array[TmplComposer]

	# Detailled list of composers
	var composer_details = new Array[TmplComposerDetail]

	# Add a composer in both lists
	fun add_composer(firstname, lastname: String, birth, death: Int)
	do
		composers.add(new TmplComposer(lastname))
		composer_details.add(new TmplComposerDetail(firstname, lastname, birth, death))
	end

	redef fun rendering do
		add """
COMPOSERS
=========
"""
		add_all composers
		add """

DETAILS
=======
"""
		add_all composer_details
	end
end

# A composer in the short list of composers
class TmplComposer
	super Template

	# Short name
	var name: String

	init(name: String) do self.name = name

	redef fun rendering do add "- {name}\n"
end

# A composer in the detailled list of composers
class TmplComposerDetail
	super Template

	var firstname: String
	var lastname: String
	var birth: Int
	var death: Int

	init(firstname, lastname: String, birth, death: Int) do
		self.firstname = firstname
		self.lastname = lastname
		self.birth = birth
		self.death = death
	end

	redef fun rendering do add """

COMPOSER: {{{firstname}}} {{{lastname}}}
BIRTH...: {{{birth}}}
DEATH...: {{{death}}}
"""

end

### Here a simple usage of the templates

var f = new TmplComposers
f.add_composer("Johann Sebastian", "Bach", 1685, 1750)
f.add_composer("George Frideric", "Handel", 1685, 1759)
f.add_composer("Wolfgang Amadeus", "Mozart", 1756, 1791)
f.write_to(stdout)
