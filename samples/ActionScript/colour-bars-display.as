package {

    import flash.display.Sprite;
    import flash.events.Event;

    public class ColourBars extends Sprite {

        public function ColourBars():void {
            if (stage) init();
            else addEventListener(Event.ADDED_TO_STAGE, init);
        }

        private function init(e:Event = null):void {

            var colours:Array = [ 0x000000, 0xFF0000, 0x00FF00, 0x0000FF, 0xFF00FF, 0x00FFFF, 0xFFFF00, 0xFFFFFF ];
            var w:Number = stage.stageWidth / 8, h:Number = stage.stageHeight;
            var x:Number = 0, i:uint, c:uint;

            for ( i = 0; i < 8; i++ ) {
                c = colours[i];
                graphics.beginFill(c);
                graphics.drawRect(w * i, 0, w, h);
            }

        }

    }

}
