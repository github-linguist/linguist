package {
	import flash.display.Sprite;

	public class MontyHall extends Sprite
	{
		public function MontyHall()
		{
			var iterations:int = 30000;
			var switchWins:int = 0;
			var stayWins:int = 0;
			
			for (var i:int = 0; i < iterations; i++)
			{
				var doors:Array = [0, 0, 0];
				doors[Math.floor(Math.random() * 3)] = 1;
				var choice:int = Math.floor(Math.random() * 3);
				var shown:int;
				
				do
				{
					shown = Math.floor(Math.random() * 3);
				} while (doors[shown] == 1 || shown == choice);
				
				stayWins += doors[choice];
				switchWins += doors[3 - choice - shown];
			}
			
			trace("Switching wins " + switchWins + " times. (" + (switchWins / iterations) * 100 + "%)");
			trace("Staying wins " + stayWins + " times. (" + (stayWins / iterations) * 100 + "%)");
		}
	}
}
