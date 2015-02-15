package  {

    import flash.display.Bitmap;
    import flash.display.BitmapData;
    import flash.display.Sprite;
    import flash.events.Event;

    public class ColourPinstripe extends Sprite {

        public function ColourPinstripe():void {
            if (stage) init();
            else addEventListener(Event.ADDED_TO_STAGE, init);
        }

        private function init(e:Event = null):void {

            var colours:Array = [ 0xFF000000, 0xFFFF0000, 0xFF00FF00, 0xFF0000FF, 0xFFFF00FF, 0xFF00FFFF, 0xFFFFFF00, 0xFFFFFFFF ];
            var data:BitmapData = new BitmapData(stage.stageWidth, stage.stageHeight, false, 0xFFFFFFFF);
            data.lock();

            var w:uint = data.width, h:uint = data.height / 4;
            var x:uint, y:uint = 0, i:uint, px:uint, colourIndex:uint, colour:uint, maxy:uint = h;

            for ( i = 1; i <= 4; i++ ) {

                for ( ; y < maxy; y++ ) {
                    colour = 0xFF000000;
                    colourIndex = 0;
                    px = 1;

                    for ( x = 0; x < w; x++ ) {

                        if ( px == i ) {
                            colourIndex = (colourIndex > 7) ? 0 : colourIndex + 1;
                            colour = colours[colourIndex];
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
