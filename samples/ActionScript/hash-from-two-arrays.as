package
{
    public class MyClass
    {
        public static function main():Void
        {
            var hash:Object = new Object();
            var keys:Array = new Array("a", "b", "c");
            var values:Array = new Array(1, 2, 3);

            for (var i:int = 0; i < keys.length(); i++)
                hash[keys[i]] = values[i];
        }
    }
}
