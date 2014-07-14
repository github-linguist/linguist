# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2012-2013 Alexis Laferrière <alexis.laf@xymus.net>
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

# Draws an arithmetic operation to the terminal
module draw_operation

redef enum Int
	fun n_chars: Int `{
		int c;
		if ( abs(recv) >= 10 )
			c = 1+(int)log10f( (float)abs(recv) );
		else
			c = 1;
		if ( recv < 0 ) c ++;
		return c;
	`}
end

redef enum Char
	fun as_operator(a, b: Int): Int
	do
		if self == '+' then return a + b
		if self == '-' then return a - b
		if self == '*' then return a * b
		if self == '/' then return a / b
		if self == '%' then return a % b
		abort
	end

	fun override_dispc: Bool
	do
		return self == '+' or self == '-' or self == '*' or self == '/' or self == '%'
	end

	fun lines(s: Int): Array[Line]
	do
		if self == '+' then
			return [new Line(new P(0,s/2),1,0,s), new Line(new P(s/2,1),0,1,s-2)]
		else if self == '-' then
			return [new Line(new P(0,s/2),1,0,s)]
		else if self == '*' then
			var lines = new Array[Line]
			for y in [1..s-1[ do
				lines.add( new Line(new P(1,y), 1,0,s-2) )
			end
			return lines
		else if self == '/' then
			return [new Line(new P(s-1,0), -1,1, s )]
		else if self == '%' then
			var q4 = s/4
			var lines = [new Line(new P(s-1,0),-1,1,s)]
			for l in [0..q4[ do
				lines.append([ new Line( new P(0,l), 1,0,q4), new Line( new P(s-1,s-1-l), -1,0,q4) ])
			end
			return lines
		else if self == '1' then
			return [new Line(new P(s/2,0), 0,1,s),new Line(new P(0,s-1),1,0,s),
				new Line( new P(s/2,0),-1,1,s/2)]
		else if self == '2' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,0),0,1,s/2),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,s/2), 0,1,s/2),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '3' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,0),0,1,s),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,s/2), 1,0,s)]
		else if self == '4' then
			return [new Line(new P(s-1,0),0,1,s), new Line( new P(0,0), 0,1,s/2),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '5' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,s/2),0,1,s/2),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,0), 0,1,s/2),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '6' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,s/2),0,1,s/2),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,0), 0,1,s),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '7' then
			var tl = new P(0,0)
			var tr = new P(s-1,0)
			return [new Line(tl, 1,0,s), new Line(tr,-1,1,s)]
		else if self == '8' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,0),0,1,s),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,0), 0,1,s),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '9' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,0),0,1,s),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,0), 0,1,s/2),
				new Line( new P(0,s/2), 1,0,s)]
		else if self == '0' then
			return [new Line(new P(0,0), 1,0,s),new Line(new P(s-1,0),0,1,s),
				new Line( new P(0,s-1),1,0,s), new Line( new P(0,0), 0,1,s)]
		end
		return new Array[Line]
	end
end

class P
	var x : Int
	var y : Int
end

redef class String
	# hack is to support a bug in the evaluation software
	fun draw(dispc: Char, size, gap: Int, hack: Bool)
	do
		var w = size * length +(length-1)*gap
		var h = size
		var map = new Array[Array[Char]]
		for x in [0..w[ do
			map[x] = new Array[Char].filled_with( ' ',  h )
		end

		var ci = 0
		for c in self.chars do
			var local_dispc
			if c.override_dispc then
				local_dispc = c
			else
				local_dispc = dispc
			end

			var lines = c.lines( size )
			for line in lines do
				var x = line.o.x+ci*size
					x += ci*gap
				var y = line.o.y
				for s in [0..line.len[ do
					assert map.length > x and map[x].length > y else print "setting {x},{y} as {local_dispc}"
					map[x][y] = local_dispc
					x += line.step_x
					y += line.step_y
				end
			end

			ci += 1
		end

		if hack then
			for c in [0..size[ do
				map[c][0] = map[map.length-size+c][0]
				map[map.length-size+c][0] = ' '
			end
		end

		for y in [0..h[ do
			for x in [0..w[ do
				printn map[x][y]
			end
			print ""
		end
	end
end

class Line
	var o : P
	var step_x : Int
	var step_y : Int
	var len : Int
end

var a
var b
var op_char
var disp_char
var disp_size
var disp_gap

if "NIT_TESTING".environ == "true" then
	a = 567
	b = 13
	op_char = '*'
	disp_char = 'O'
	disp_size = 8
	disp_gap = 1
else
	printn "Left operand: "
	a = gets.to_i

	printn "Right operand: "
	b = gets.to_i

	printn "Operator (+, -, *, /, %): "
	op_char = gets.chars[0]

	printn "Char to display: "
	disp_char = gets.chars[0]

	printn "Size of text: "
	disp_size = gets.to_i

	printn "Space between digits: "
	disp_gap = gets.to_i
end

var result = op_char.as_operator( a, b )

var len_a = a.n_chars
var len_b = b.n_chars
var len_res = result.n_chars
var max_len = len_a.max( len_b.max( len_res ) ) + 1

# draw first line
var d = max_len - len_a
var line_a = ""
for i in [0..d[ do line_a += " "
line_a += a.to_s
line_a.draw( disp_char, disp_size, disp_gap, false )

print ""
# draw second line
d = max_len - len_b-1
var line_b = op_char.to_s
for i in [0..d[ do line_b += " "
line_b += b.to_s
line_b.draw( disp_char, disp_size, disp_gap, false )

# draw -----
print ""
for i in [0..disp_size*max_len+(max_len-1)*disp_gap] do
	printn "_"
end
print ""
print ""

# draw result
d = max_len - len_res
var line_res = ""
for i in [0..d[ do line_res += " "
line_res += result.to_s
line_res.draw( disp_char, disp_size, disp_gap, false )
