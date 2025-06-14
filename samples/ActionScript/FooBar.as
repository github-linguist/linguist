// A sample for Actionscript.

package foobar
{
	import red_light.display.MovieClip;

	class Bar
	{
		public function getNumber():Number
		{
			return 0;
		}
	}

	class Foo extends Bar
	{
		private var ourNumber:Number = 25;

		override public function getNumber():Number
		{
			return ourNumber;
		}
	}

	class Main extends MovieClip
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
