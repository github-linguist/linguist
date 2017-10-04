// A sample for Actionscript.

package foobar
{
	import flash.display.MovieClip;

	public class Bar
	{
		public function getNumber():Number
		{
			return 10;
		}
	}

	public class Foo extends Bar
	{
		private var ourNumber:Number = 25;

		override public function getNumber():Number
		{
			return ourNumber;
		}
	}

	public class Main extends MovieClip
	{
		public function Main()
		{
			var x:Bar = new Bar();
			var y:Foo = new Foo();
			trace(x.getNumber());
			trace(y.getNumber());
		}
	}
}
