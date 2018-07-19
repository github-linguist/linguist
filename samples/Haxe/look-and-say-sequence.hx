using Std;

class Main
{
	
	static function main()
	{
		var test = "1";
		for (i in 0...11) {
			Sys.println(test);
			test = lookAndSay(test);
		}
	}

	static function lookAndSay(s:String)
	{
		if (s == null || s == "") return "";
		
		var results = "";
		var repeat = s.charAt(0);
		var amount = 1;
		for (i in 1...s.length)
		{
			var actual = s.charAt(i);
			if (actual != repeat)
			{
				results += amount.string();
				results += repeat;
				repeat = actual;
				amount = 0;
			}
			amount++;
		}
		results += amount.string();
		results += repeat;
		
		return results;
	}
}
