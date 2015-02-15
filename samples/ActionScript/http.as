package
{
    import flash.display.Sprite;
    import flash.events.Event;
    import flash.net.*;

    public class RequestExample extends Sprite
    {
        public function RequestExample()
        {
            var loader:URLLoader = new URLLoader();
            loader.addEventListener(Event.COMPLETE, loadComplete);
            loader.load(new URLRequest("http://www.rosettacode.org"));
        }
		
        private function loadComplete(evt:Event):void
        {
            trace(evt.target.data);
        }
    }
}
