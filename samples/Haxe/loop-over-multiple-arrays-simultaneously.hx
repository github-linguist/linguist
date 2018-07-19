using Lambda;
using Std;

class Main
{
	
	static function main()
	{
		var a = ['a', 'b', 'c'];
		var b = ['A', 'B', 'C'];
		var c = [1, 2, 3];
		
		//Find smallest array
		var len = [a, b, c]
			.map(function(a) return a.length)
			.fold(Math.min, 0x0FFFFFFF)
			.int();

		for (i in 0...len)
			Sys.println(a[i] + b[i] + c[i].string());
	}
}
