package
{
    import loom.Application;

    public interface I {}
    public class C {}
    public class B extends C implements I {}
    final public class A extends B {}

    delegate ToCompute(s:String, o:Object):Number;

    public enum Enumeration
    {
      foo,
      baz,
      cat,
    }

    struct P {
        public var x:Number = 0;
        public var y:Number = 0;
        public static operator function =(a:P, b:P):P
        {
            a.x = b.x;
            a.y = b.y;

            return a;
        }
    }

    // single-line comment

    /*
    Multi-line comment
    */

    /**
    Doc comment
    */
    public class SyntaxExercise extends Application
    {
        static public var classVar:String = 'class variable';
        public const CONST:String = 'constant';
        private var _a:A = new A();
        public var _d:ToCompute;

        override public function run():void
        {
            trace("hello");
        }

        private function get a():A { return _a; }
        private function set a(value:A):void { _a = value; }

        private function variousTypes(defaultValue:String = ''):void
        {
            var nil:Object = null;
            var b1:Boolean = true;
            var b2:Boolean = false;
            var n1:Number = 0.123;
            var n2:Number = 12345;
            var n3:Number = 0xfed;
            var s1:String = 'single-quotes with "quotes" inside';
            var s2:String = "double-quotes with 'quotes' inside";
            var f1:Function = function (life:String, universe:Object, ...everything):Number { return 42; };
            var v1:Vector.<Number> = [1, 2];
            var d1:Dictionary.<String, Number> = { 'three': 3, 'four': 4 };

            _d += f1;
            _d -= f1;
        }

        private function variousOps():void
        {
            var a = ((100 + 200 - 0) / 300) % 2;
            var b = 100 * 30;
            var d = true && (b > 301);
            var e = 0x10 | 0x01;

            b++; b--;
            a += 300; a -= 5; a *= 4; a /= 2; a %= 7;

            var castable1:Boolean = (a is B);
            var castable2:Boolean = (a as B) != null;
            var cast:String = B(a).toString();
            var instanced:Boolean = (_a instanceof A);
        }

        private function variousFlow():void
        {
            var n:Number = Math.random();
            if (n > 0.6)
                trace('top 40!');
            else if(n > 0.3)
                trace('mid 30!');
            else
                trace('bottom 30');

            var flip:String =  (Math.random() > 0.5) ? 'heads' : 'tails';

            for (var i = 0; i < 100; i++)
                trace(i);

            var v:Vector.<String> = ['a', 'b', 'c'];
            for each (var s:String in v)
                trace(s);

            var d:Dictionary.<String, Number> = { 'one': 1 };
            for (var key1:String in d)
                trace(key1);

            for (var key2:Number in v)
                trace(key2);

            while (i > 0)
            {
                i--;
                if (i == 13) continue;
                trace(i);
            }

            do
            {
                i++;
            }
            while (i < 10);

            switch (Math.floor(Math.random()) * 3 + 1)
            {
                case 1 : trace('rock'); break;
                case 2 : trace('paper'); break;
                default: trace('scissors'); break;
            }
        }

    }
}
