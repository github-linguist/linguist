package {
	
    import flash.display.Sprite;
    import flash.events.Event;

    public class VanDerCorput extends Sprite {

        public function VanDerCorput():void {
            if (stage) init();
            else addEventListener(Event.ADDED_TO_STAGE, init);
        }

        private function init(e:Event = null):void {

            removeEventListener(Event.ADDED_TO_STAGE, init);

            var base2:Vector.<Number> = new Vector.<Number>(10, true);
            var base3:Vector.<Number> = new Vector.<Number>(10, true);
            var base4:Vector.<Number> = new Vector.<Number>(10, true);
            var base5:Vector.<Number> = new Vector.<Number>(10, true);
            var base6:Vector.<Number> = new Vector.<Number>(10, true);
            var base7:Vector.<Number> = new Vector.<Number>(10, true);
            var base8:Vector.<Number> = new Vector.<Number>(10, true);

            var i:uint;

            for ( i = 0; i < 10; i++ ) {
                base2[i] = Math.round( _getTerm(i, 2) * 1000000 ) / 1000000;
                base3[i] = Math.round( _getTerm(i, 3) * 1000000 ) / 1000000;
                base4[i] = Math.round( _getTerm(i, 4) * 1000000 ) / 1000000;
                base5[i] = Math.round( _getTerm(i, 5) * 1000000 ) / 1000000;
                base6[i] = Math.round( _getTerm(i, 6) * 1000000 ) / 1000000;
                base7[i] = Math.round( _getTerm(i, 7) * 1000000 ) / 1000000;
                base8[i] = Math.round( _getTerm(i, 8) * 1000000 ) / 1000000;
            }

            trace("Base 2: " + base2.join(', '));
            trace("Base 3: " + base3.join(', '));
            trace("Base 4: " + base4.join(', '));
            trace("Base 5: " + base5.join(', '));
            trace("Base 6: " + base6.join(', '));
            trace("Base 7: " + base7.join(', '));
            trace("Base 8: " + base8.join(', '));

        }

        private function _getTerm(n:uint, base:uint = 2):Number {

            var r:Number = 0, p:uint, digit:uint;
            var baseLog:Number = Math.log(base);

            while ( n > 0 ) {
                p = Math.pow( base, uint(Math.log(n) / baseLog) );

                digit = n / p;
                n %= p;
                r += digit / (p * base);
            }

            return r;

        }

    }

}
