class Main
{
	static function main()
	{
		triangle(3);
	}
	
	static inline var SPACE = ' ';
	static inline var STAR = '*';
	
	static function triangle(o) {
		var n = 1 << o;
		var line = new Array<String>();
		
		for (i in 0...(n*2)) line[i] = SPACE;
		
		line[n] = '*';
		
		for (i in 0...n) {
			Sys.println(line.join(''));
			var u ='*';
			var start = n - i;
			var end = n + i + 1;
			var t = SPACE;
			for (j in start...end) {
				t = (line[j-1] == line[j+1] ? SPACE : STAR);
				line[j-1] = u;
				u = t;
			}
			
			line[n+i] = t;
			line[n+i+1] = STAR;
		}
	}
}
