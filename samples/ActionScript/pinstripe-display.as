package  {

    import flash.display.Bitmap;
    import flash.display.BitmapData;
    import flash.display.Sprite;
    import flash.events.Event;

    public class Pinstripe extends Sprite {

        public function Pinstripe():void {
            if (stage) init();
            else addEventListener(Event.ADDED_TO_STAGE, init);
        }

        private function init(e:Event = null):void {

            var data:BitmapData = new BitmapData(stage.stageWidth, stage.stageHeight, false, 0xFFFFFFFF);
            data.lock();

            var w:uint = data.width, h:uint = data.height / 4;
            var x:uint, y:uint = 0, i:uint, px:uint, colour:uint, maxy:uint = h;

            for ( i = 1; i <= 4; i++ ) {

                for ( ; y < maxy; y++ ) {
                    colour = 0xFF000000;
                    px = 1;

                    for ( x = 0; x < w; x++ ) {

                        if ( px == i ) {
                            colour = (colour == 0xFF000000) ? 0xFFFFFFFF : 0xFF000000;
                            px = 1;
                        }
                        else px++;

                        data.setPixel32(x, y, colour);

                    }
                }

                maxy += h;

            }

            data.unlock();
            addChild(new Bitmap(data));
        }

    }

}
