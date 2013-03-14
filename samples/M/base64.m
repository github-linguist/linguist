	;
	; This file is part of DataBallet.
	; Copyright (C) 2012 Laurent Parenteau <laurent.parenteau@gmail.com>
	;
	; DataBallet is free software: you can redistribute it and/or modify
	; it under the terms of the GNU Affero General Public License as
	; published by the Free Software Foundation, either version 3 of the
	; License, or (at your option) any later version.
	;
	; DataBallet is distributed in the hope that it will be useful,
	; but WITHOUT ANY WARRANTY; without even the implied warranty of
	; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	; GNU Affero General Public License for more details.
	;
	; You should have received a copy of the GNU Affero General Public License
	; along with DataBallet.  If not, see <http://www.gnu.org/licenses/>.
	;

encode(message)
	;
	; Return base64 with URL and Filename safe alphabet (RFC 4648)
	;
	new base64,todrop,i
	
	; Populate safe alphabet values on first use only.
	if '$data(base64safe) do
	.	for i=0:1:25 set base64safe(i)=$zchar(65+i),base64safe(i+26)=$zchar(97+i)
	.	for i=52:1:61 set base64safe(i)=$zchar(i-4)
	.	set base64safe(62)="-",base64safe(63)="_"

	; Pad message with 0 to ensure number of bytes is a multiple of 3.
	set todrop=0
	for  quit:($zlength(message)#3)=0  set message=message_$zchar(0) set todrop=todrop+1

	; Base64 encode the message
	set base64=""
	for i=1:3:$zlength(message) do
	.	set base64=base64_base64safe($zascii(message,i)\4)
	.	set base64=base64_base64safe(($zascii(message,i)#4*16)+($zascii(message,i+1)\16))
	.	set base64=base64_base64safe(($zascii(message,i+1)#16*4)+($zascii(message,i+2)\64))
	.	set base64=base64_base64safe($zascii(message,i+2)#64)

	set:todrop'=0 base64=$zextract(base64,1,$zlength(base64)-todrop)

	quit base64
