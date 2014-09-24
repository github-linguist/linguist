# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2014 Lucas Bajolet <r4pass@hotmail.com>
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

# Sample module for a minimal chat server using Websockets on port 8088
module websocket_server

import websocket

var sock = new WebSocket(8088, 1)

var msg: String

if sock.listener.eof then
	print sys.errno.strerror
end

sock.accept

while not sock.listener.eof do
	if not sock.connected then sock.accept
	if sys.stdin.poll_in then
		msg = gets
		printn "Received message : {msg}"
		if msg == "exit" then sock.close
		if msg == "disconnect" then sock.disconnect_client
		sock.write(msg)
	end
	if sock.can_read(10) then
		msg = sock.read_line
		if msg != "" then print msg
	end
end

