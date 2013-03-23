	;
	; This file is part of DataBallet.
	; Copyright (C) 2012 Laurent Parenteau <laurent.parenteau@gmail.com>
	;
	; DataBallet is free software: you can redistribute it and/or modify
	; it under the terms of the GNU Affero General Public License as published by
	; the Free Software Foundation, either version 3 of the License, or
	; (at your option) any later version.
	;
	; DataBallet is distributed in the hope that it will be useful,
	; but WITHOUT ANY WARRANTY; without even the implied warranty of
	; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
	; GNU Affero General Public License for more details.
	;
	; You should have received a copy of the GNU Affero General Public License
	; along with DataBallet. If not, see <http://www.gnu.org/licenses/>.
	;

decode(val)
	;
	; Decoded a URL Encoded string
	;
	new decoded,c,i

	set decoded=""
	for i=1:1:$zlength(val) do
	.	set c=$zextract(val,i,i)
	.	if c="+" set decoded=decoded_" "
	.	else  if c'="%" set decoded=decoded_c
	.	else  set decoded=decoded_$zchar($$FUNC^%HD($zextract(val,i+1,i+2))) set i=i+2
	quit decoded

encode(val)
	;
	; Encoded a string for URL usage
	;
	new encoded,c,i
	set encoded=""

	; Populate safe char only the first time
	if '$data(safechar) for i=45,46,95,126,48:1:57,65:1:90,97:1:122 set safechar($zchar(i))=""

	for i=1:1:$zlength(val) do
	.	set c=$zextract(val,i,i)
	.	if $data(safechar(c)) set encoded=encoded_c
	.	else  if c=" " set encoded=encoded_"+"
	.	else  set encoded=encoded_"%"_$$FUNC^%DH($zascii(c),2)

	quit encoded
