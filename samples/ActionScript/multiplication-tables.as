package {

    import flash.display.Sprite;
    import flash.events.Event;
    import flash.text.TextField;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFormat;

    [SWF (width = 550, height = 550)]
    public class MultiplicationTable extends Sprite {

        public function MultiplicationTable() {
            if ( stage ) _init();
            else addEventListener(Event.ADDED_TO_STAGE, _init);
        }

        private function _init(e:Event = null):void {

            removeEventListener(Event.ADDED_TO_STAGE, _init);

            var format:TextFormat = new TextFormat();
            format.size = 15;
            var blockSize:uint = 40;
            var max:uint = 12;

            var i:uint, j:uint;
            var tf:TextField;

            for ( i = 1; i <= max; i++ ) {
                tf = new TextField();
                tf.defaultTextFormat = format;
                tf.x = blockSize * i;
                tf.y = 0;
                tf.width = tf.height = blockSize;
                tf.autoSize = TextFieldAutoSize.CENTER;
                tf.text = String(i);
                addChild(tf);

                tf = new TextField();
                tf.defaultTextFormat = format;
                tf.x = 0;
                tf.y = blockSize * i;
                tf.width = tf.height = blockSize;
                tf.autoSize = TextFieldAutoSize.CENTER;
                tf.text = String(i);
                addChild(tf);
            }

            var yOffset:Number = tf.textHeight / 2;
            y += yOffset;

            graphics.lineStyle(1, 0x000000);
            graphics.moveTo(blockSize, -yOffset);
            graphics.lineTo(blockSize, (blockSize * (max + 1)) - yOffset);
            graphics.moveTo(0, blockSize - yOffset);
            graphics.lineTo(blockSize * (max + 1), blockSize - yOffset);


            for ( i = 1; i <= max; i++ ) {
                for ( j = 1; j <= max; j++ ) {
                    if ( j > i )
                        continue;

                    tf = new TextField();
                    tf.defaultTextFormat = format;
                    tf.x = blockSize * i;
                    tf.y = blockSize * j;
                    tf.width = tf.height = blockSize;
                    tf.autoSize = TextFieldAutoSize.CENTER;
                    tf.text = String(i * j);
                    addChild(tf);
                }
            }

        }

    }

}
