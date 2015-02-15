package
{
    public class ArrayCallback
    {
        public function main():void
        {
            var nums:Array = new Array(1, 2, 3);
            nums.map(function(n:Number, index:int, arr:Array):void { trace(n * n * n); });

            // You can also pass a function reference
            nums.map(cube);
        }

        private function cube(n:Number, index:int, arr:Array):void
        {
            trace(n * n * n);
        }
    }
}
