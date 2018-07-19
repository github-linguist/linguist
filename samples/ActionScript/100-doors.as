package {
    import flash.display.Sprite;

    public class Doors extends Sprite {
        public function Doors() {

            // Initialize the array
            var doors:Array = new Array(100);
            for (var i:Number = 0; i < 100; i++) {
                doors[i] = false;

            // Do the work
            for (var pass:Number = 0; pass < 100; pass++) {
                for (var j:Number = pass; j < 100; j += (pass+1)) {
                    doors[j] = !doors[j];
                }
            }
            trace(doors);
        }
    }
}
