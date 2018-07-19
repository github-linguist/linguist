package  {

    import flash.display.Sprite;
    import flash.events.Event;
    import flash.sampler.getSize;

    public class VariableSizeGet extends Sprite {

        public function VariableSizeGet() {
            if ( stage ) _init();
            else addEventListener(Event.ADDED_TO_STAGE, _init);
        }

        private function _init(e:Event = null):void {

            var i:int = 1;
            var n:Number = 0.5;
            var s:String = "abc";
            var b:Boolean = true;
            var date:Date = new Date();

            trace("An int contains " + getSize(i) + " bytes.");  // 4
            trace("A Number contains " + getSize(n) + " bytes.");  // 8
            trace("The string 'abc' contains " + getSize(s) + " bytes.");  // 24
            trace("A Boolean contains " + getSize(b) + " bytes.");  // 4
            trace("A Date object contains " + getSize(date) + " bytes.");  // 48

        }

    }

}
