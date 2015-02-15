package
{
	import flash.display.Sprite;
	
	[SWF(width="640", height="480")]
	public class GreyscaleBars extends Sprite
	{
		
		public function GreyscaleBars()
		{
			_drawRow(8, 0);
			_drawRow(16, stage.stageHeight/4, true);
			_drawRow(32, stage.stageHeight/2);
			_drawRow(64, stage.stageHeight/4 * 3, true);
		}
		
		private function _drawRow(nbSteps : uint, startingY : uint, reverse : Boolean = false) : void {
			
			for (var i : int = 0; i < nbSteps; i++) {
				graphics.beginFill(0x00, reverse ? 1 - (i/nbSteps) : (i/nbSteps));
				graphics.drawRect(i * stage.stageWidth / nbSteps, startingY, stage.stageWidth/nbSteps, stage.stageHeight/4);
				graphics.endFill();
			}
		}
	}
}
