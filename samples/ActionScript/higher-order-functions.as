package {
    public class MyClass {

        public function first(func:Function):String {
            return func.call();
        }

        public function second():String {
            return "second";
        }

        public static function main():void {
            var result:String = first(second);
            trace(result);
            result = first(function() { return "third"; });
            trace(result);
        }
    }
}
