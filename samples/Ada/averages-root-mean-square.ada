with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
procedure calcrms is
	type float_arr is array(1..10) of Float;
	
	function rms(nums : float_arr) return Float is
		sum : Float := 0.0;
		begin
		for p in nums'Range loop
			sum := sum + nums(p)**2;
		end loop;
		return sqrt(sum/Float(nums'Length));
	end rms;

	list : float_arr;
begin
list := (1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0);
put( rms(list) , Exp=>0);
end calcrms;
