# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Alexis Laferrière <alexis.laf@xymus.net>
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

# Example using the privileges module to drop privileges from root
module drop_privileges

import privileges

# basic command line options
var opts = new OptionContext
var opt_ug = new OptionUserAndGroup.for_dropping_privileges
opt_ug.mandatory = true
opts.add_option(opt_ug)

# parse and check command line options
opts.parse(args)
if not opts.errors.is_empty then
	print opts.errors
	print "Usage: drop_privileges [options]"
	opts.usage
	exit 1
end

# original user
print "before {sys.uid}:{sys.gid}"

# make the switch
var user_group = opt_ug.value
assert user_group != null
user_group.drop_privileges

# final user
print "after {sys.uid}:{sys.egid}"
