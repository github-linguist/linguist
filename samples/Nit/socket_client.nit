# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2013 Matthieu Lucas <lucasmatthieu@gmail.com>
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

# Client sample using the Socket module which connect to the server sample.
module socket_client

import socket

if args.length < 2 then
	print "Usage : socket_client <host> <port>"
	return
end

var s = new Socket.client(args[0], args[1].to_i)
print "[HOST ADDRESS] : {s.address}"
print "[HOST] : {s.host}"
print "[PORT] : {s.port}"
print "Connecting ... {s.connected}"
if s.connected then
	print "Writing ... Hello server !"
	s.write("Hello server !")
	print "[Response from server] : {s.read(100)}"
	print "Closing ..."
	s.close
end
