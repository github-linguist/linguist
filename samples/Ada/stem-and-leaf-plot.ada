with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Gnat.Heap_Sort_G;
procedure stemleaf is
	data : array(Natural Range <>) of Integer := (
	0,12,127,28,42,39,113, 42,18,44,118,44,37,113,124,37,48,127,36,29,31,
	125,139,131,115,105,132,104,123,35,113,122,42,117,119,58,109,23,105,
	63,27,44,105,99,41,128,121,116,125,32,61,37,127,29,113,121,58,114,126,
	53,114,96,25,109,7,31,141,46,13,27,43,117,116,27,7,68,40,31,115,124,42,
	128,52,71,118,117,38,27,106,33,117,116,111,40,119,47,105,57,122,109,
	124,115,43,120,43,27,27,18,28,48,125,107,114,34,133,45,120, 30,127,
	31,116,146); -- Position 0 is used for storage during sorting, initialized as 0

	procedure Move (from, to : in Natural) is
	begin data(to) := data(from);
	end Move;
	
	function Cmp (p1, p2 : Natural) return Boolean is
	begin return data(p1)<data(p2);
	end Cmp;
	
	package Sorty is new GNAT.Heap_Sort_G(Move,Cmp);
	min,max,p,stemw: Integer;
begin
	Sorty.Sort(data'Last);
	min := data(1);
	max := data(data'Last);
	stemw := Integer'Image(max)'Length;
	p := 1;
	for stem in min/10..max/10 loop
		put(stem,Width=>stemw); put(" |");
		Leaf_Loop:
			while data(p)/10=stem loop
				put(" "); put(data(p) mod 10,Width=>1);
				exit Leaf_loop when p=data'Last;
				p := p+1;
		end loop Leaf_Loop;
		new_line;
	end loop;
end stemleaf;
