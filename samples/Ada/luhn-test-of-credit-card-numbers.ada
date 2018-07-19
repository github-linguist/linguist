with Ada.Text_IO; use Ada.Text_IO;
procedure luhn is
	function luhn_test(num : String) return Boolean is
	sum : Integer := 0;
	odd : Boolean := True;
	int : Integer;
	begin
	for p in reverse num'Range loop
		int := Integer'Value(num(p..p));
		if odd then
			sum := sum + int;
		else
			sum := sum + (int*2 mod 10) + (int / 5);
		end if;
		odd := not odd;
	end loop;
	return (sum mod 10)=0;
	end luhn_test;

begin
put_line(Boolean'Image(luhn_test("49927398716")));
put_line(Boolean'Image(luhn_test("49927398717")));
put_line(Boolean'Image(luhn_test("1234567812345678")));
put_line(Boolean'Image(luhn_test("1234567812345670")));
end luhn;
