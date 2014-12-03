package
{
    import loom.Application;
    import loom2d.display.StageScaleMode;
    import loom2d.ui.SimpleLabel;

    /**
    The HelloWorld app renders a label with its name on it,
    and traces 'hello' to the log.
    */
    public class HelloWorld extends Application
    {

        override public function run():void
        {
            stage.scaleMode = StageScaleMode.LETTERBOX;
            centeredMessage(simpleLabel, this.getFullTypeName());

            trace("hello");
        }

        // a convenience getter that generates a label and adds it to the stage
        private function get simpleLabel():SimpleLabel
        {
            return stage.addChild(new SimpleLabel("assets/Curse-hd.fnt")) as SimpleLabel;
        }

        // a utility to set the label's text and then center it on the stage
        private function centeredMessage(label:SimpleLabel, msg:String):void
        {
            label.text = msg;
            label.center();
            label.x = stage.stageWidth / 2;
            label.y = (stage.stageHeight / 2) - (label.height / 2);
        }

    }
}
