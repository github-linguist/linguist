package {
	import flash.display.Sprite;

	public class SumAndProduct extends Sprite
	{
		public function SumAndProduct()
		{
			var arr:Array = [1, 2, 3, 4, 5];
			var sum:int = 0;
			var prod:int = 1;
			
			for (var i:int = 0; i < arr.length; i++)
			{
				sum += arr[i];
				prod *= arr[i];
			}
			
			trace("Sum: " + sum); // 15
			trace("Product: " + prod); // 120
		}
	}
}
