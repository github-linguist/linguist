package  {
	
    import flash.display.Sprite;
    import flash.events.Event;
    import flash.text.TextField;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFormat;
	
    public class MousePosition extends Sprite {

        private var _textField:TextField = new TextField();

        public function MousePosition() {
            if ( stage ) init();
            else addEventListener(Event.ADDED_TO_STAGE, init);
        }

        private function init(e:Event = null):void {
            removeEventListener(Event.ADDED_TO_STAGE, init);

            _textField.autoSize = TextFieldAutoSize.RIGHT;
            _textField.x = stage.stageWidth - 10;
            _textField.defaultTextFormat = new TextFormat(null, 15);
            _textField.text = "Mouse position: X = 0, Y = 0";
            _textField.y = stage.stageHeight - _textField.textHeight - 14;
            addChild(_textField);

            addEventListener(Event.ENTER_FRAME, _onEnterFrame);
        }

        private function _onEnterFrame(e:Event):void {
            _textField.text = "Mouse position: X = " + stage.mouseX + ", Y = " + stage.mouseY;
        }

    }

}
