with Ada.Text_IO; use Ada.Text_IO;
procedure dot_product is
	type vect is array(Positive range <>) of Integer;
	v1 : vect := (1,3,-5);
	v2 : vect := (4,-2,-1);

	function dotprod(a: vect; b: vect) return Integer is
		sum : Integer := 0;
		begin
		if not (a'Length=b'Length) then raise Constraint_Error; end if;
		for p in a'Range loop
			sum := sum + a(p)*b(p);
		end loop;
		return sum;
	end dotprod;
	
begin
put_line(Integer'Image(dotprod(v1,v2)));
end dot_product;
