package {

    import flash.display.Graphics;
    import flash.display.Shape;
    import flash.display.Sprite;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import flash.text.TextField;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFormat;

    public class PigTheDiceGame extends Sprite {

        /**
         * The name of the first player.
         *
         * @private
         */
        private var _name1:String = "Player 1";

        /**
         * The name of the second player.
         *
         * @private
         */
        private var _name2:String = "Player 2";

        /**
         * True if the next turn is of the second player, false if it is of the first player.
         *
         * @private
         */
        private var _isPlayer2:Boolean = false;

        /**
         * The score of the first player.
         *
         * @private
         */
        private var __p1Score:uint;

        /**
         * The score of the second player.
         *
         * @private
         */
        private var __p2Score:uint;

        /**
         * The number of points in the current turn.
         *
         * @private
         */
        private var __turnPts:uint;

        /**
         * The text field displaying the score of the first player.
         *
         * @private
         */
        private var _p1ScoreText:TextField;

        /**
         * The text field displaying the score of the second player.
         *
         * @private
         */
        private var _p2ScoreText:TextField;

        /**
         * The button which must be clicked for a player to roll the dice.
         *
         * @private
         */
        private var _rollButton:Sprite;

        /**
         * The button which must be clicked for a player to hold.
         *
         * @private
         */
        private var _holdButton:Sprite;

        /**
         * The text field displaying the name of the current player.
         */
        private var _currentPlayerText:TextField;

        /**
         * The text field displaying the number of points in the current turn.
         *
         * @private
         */
        private var _ptsThisTurnText:TextField;

        /**
         * The dice.
         *
         * @private
         */
        private var _dice:Shape;

        /**
         * The number of points required to win the game.
         *
         * @private
         */
        private var _maxScore:uint = 100;

        /**
         * The text field displaying additional information about the game.
         *
         * @private
         */
        private var _statusText:TextField;

        /**
         * Creates a new PigTheDiceGame instance.
         */
        public function PigTheDiceGame() {
            if ( stage ) _init();
            else addEventListener(Event.ADDED_TO_STAGE, _init);
        }

        /**
         * Function which constructs the dice game when the object is added to the stage.
         *
         * @private
         */
        private function _init(e:Event = null):void {

            // Border and background

            graphics.beginFill(0xFFFFDD);
            graphics.lineStyle(2, 0xFFCC00);
            graphics.drawRect(0, 0, 450, 280)

            x = 20;
            y = 20;

            // Text fields and labels

            var currentPlayerText:TextField = _createTextField(_name1 + "'s turn", 20, 0, 10, 0xDD0000, TextFieldAutoSize.CENTER, width);
            var p1ScoreLabel:TextField = _createTextField(_name1 + "'s score:", 15, 20, currentPlayerText.y + currentPlayerText.height + 20, 0x000000, TextFieldAutoSize.LEFT, 120);
            var p1ScoreText:TextField = _createTextField("0", 17, 135, p1ScoreLabel.y, 0xFF0000, TextFieldAutoSize.RIGHT, 50);
            var p2ScoreLabel:TextField = _createTextField(_name2 + "'s score:", 15, 20, p1ScoreText.y + p1ScoreText.height + 5, 0x000000, TextFieldAutoSize.LEFT, 120);
            var p2ScoreText:TextField = _createTextField("0", 17, 135, p2ScoreLabel.y, 0xFF0000, TextFieldAutoSize.RIGHT, 50);
            var ptsThisTurnLabel:TextField = _createTextField("Points in this turn:", 15, 20, p2ScoreText.y + p2ScoreText.height + 15, 0x000000, TextFieldAutoSize.LEFT, 120);
            var ptsThisTurnText:TextField = _createTextField("0", 17, 135, ptsThisTurnLabel.y, 0xFF0000, TextFieldAutoSize.RIGHT, 50);

            // Dice

            var dice:Shape = new Shape();
            dice.x = 201;
            dice.y = ptsThisTurnText.y + ptsThisTurnText.height + 30;
            dice.visible = false;

            var statusText:TextField = _createTextField("Start Play!", 15, 0, dice.y + 70, 0x0000A0, TextFieldAutoSize.CENTER, 450);

            // "Roll" button

            var rollButton:Sprite = new Sprite();
            var rollButtonText:TextField = _createTextField("Roll dice", 17, 0, 0, 0x000000, TextFieldAutoSize.CENTER, 100);
            rollButton.mouseChildren = false;
            rollButton.buttonMode = true;
            rollButton.graphics.lineStyle(1, 0x444444);
            rollButton.graphics.beginFill(0xDDDDDD);
            rollButton.graphics.drawRect(0, 0, 100, rollButtonText.height);
            rollButton.x = 330;
            rollButton.y = 80;
            rollButton.addChild(rollButtonText);

            // "Hold" button

            var holdButton:Sprite = new Sprite();
            var holdButtonText:TextField = _createTextField("Hold", 17, 0, 0, 0x000000, TextFieldAutoSize.CENTER, 100);
            holdButton.mouseChildren = false;
            holdButton.buttonMode = true;
            holdButton.graphics.copyFrom(rollButton.graphics);
            holdButton.x = 330;
            holdButton.y = rollButton.y + rollButton.height + 10;
            holdButton.addChild(holdButtonText);

            rollButton.addEventListener(MouseEvent.CLICK, _rollButtonClick);
            holdButton.addEventListener(MouseEvent.CLICK, _holdButtonClick);

            _currentPlayerText = currentPlayerText;
            _p1ScoreText = p1ScoreText;
            _p2ScoreText = p2ScoreText;
            _ptsThisTurnText = ptsThisTurnText;
            _rollButton = rollButton;
            _holdButton = holdButton;
            _dice = dice;
            _statusText = statusText;

            addChild(currentPlayerText);
            addChild(p1ScoreLabel);
            addChild(p1ScoreText);
            addChild(p2ScoreLabel);
            addChild(p2ScoreText);
            addChild(ptsThisTurnLabel);
            addChild(ptsThisTurnLabel);
            addChild(ptsThisTurnText);
            addChild(statusText);
            addChild(_dice);
            addChild(rollButton);
            addChild(holdButton);

        }

        /**
         * Creates a new text field.
         *
         * @param text The text to be displayed in the text field.
         * @param size The font size of the text.
         * @param x The x-coordinate of the text field.
         * @param y The y-coordinate of the text field.
         * @param colour The text colour.
         * @param autoSize The text alignment mode.
         * @param width The width of the text field.
         * @return A TextField object.
         * @private
         */
        private function _createTextField(text:String, size:Number, x:Number, y:Number, colour:uint, autoSize:String, width:Number):TextField {
            var t:TextField = new TextField();
            t.defaultTextFormat = new TextFormat(null, size, colour);
            t.autoSize = autoSize;
            t.x = x;
            t.y = y;
            t.width = width;
            t.text = text;
            t.height = t.textHeight + 5;
            return t;
        }

        /**
         * Rolls the dice.
         *
         * @return The result of the roll (1-6)
         * @private
         */
        private function _rollDice():uint {

            // Since Math.random() returns a number between 0 and 1, multiplying it by 6 and then rounding down
            // gives a number between 0 and 5, so add 1 to it.

            var roll:uint = uint(Math.random() * 6) + 1;

            _dice.visible = true;
            var diceGraphics:Graphics = _dice.graphics;

            // Draw the dice.

            diceGraphics.clear();
            diceGraphics.lineStyle(2, 0x555555);
            diceGraphics.beginFill(0xFFFFFF);
            diceGraphics.drawRect(0, 0, 48, 48);
            diceGraphics.beginFill(0x000000);
            diceGraphics.lineStyle(0);

            switch ( roll ) {
                case 1:
                    diceGraphics.drawCircle(24, 24, 3);
                    break;
                case 2:
                    diceGraphics.drawCircle(16, 16, 3);
                    diceGraphics.drawCircle(32, 32, 3);
                    break;
                case 3:
                    diceGraphics.drawCircle(12, 12, 3);
                    diceGraphics.drawCircle(24, 24, 3);
                    diceGraphics.drawCircle(36, 36, 3);
                    break;
                case 4:
                    diceGraphics.drawCircle(16, 16, 3);
                    diceGraphics.drawCircle(16, 32, 3);
                    diceGraphics.drawCircle(32, 16, 3);
                    diceGraphics.drawCircle(32, 32, 3);
                    break;
                case 5:
                    diceGraphics.drawCircle(12, 12, 3);
                    diceGraphics.drawCircle(24, 24, 3);
                    diceGraphics.drawCircle(36, 36, 3);
                    diceGraphics.drawCircle(36, 12, 3);
                    diceGraphics.drawCircle(12, 36, 3);
                    break;
                case 6:
                    diceGraphics.drawCircle(16, 12, 3);
                    diceGraphics.drawCircle(16, 24, 3);
                    diceGraphics.drawCircle(16, 36, 3);
                    diceGraphics.drawCircle(32, 12, 3);
                    diceGraphics.drawCircle(32, 24, 3);
                    diceGraphics.drawCircle(32, 36, 3);
                    break;
            }

            return roll;

        }

        /**
         * The score of the first player.
         *
         * @private
         */
        private function get _p1Score():uint {
            return __p1Score;
        }

        /**
         * @private
         */
        private function set _p1Score(value:uint):void {
            __p1Score = value;
            _p1ScoreText.text = String(value);

            if ( value >= _maxScore ) {
                _currentPlayerText.text = "Game over!";
                _statusText.text = _name1 + " wins!";
                removeChild(_rollButton);
                removeChild(_holdButton);
            }
        }

        /**
         * The score of the second player.
         *
         * @private
         */
        private function get _p2Score():uint {
            return __p2Score;
        }

        /**
         * @private
         */
        private function set _p2Score(value:uint):void {
            __p2Score = value;
            _p2ScoreText.text = String(value);

            if ( value >= _maxScore ) {
                _currentPlayerText.text = "Game over!";
                _statusText.text = _name2 + " wins!";
                removeChild(_rollButton);
                removeChild(_holdButton);
            }
        }

        /**
         * The number of points in the current turn.
         *
         * @private
         */
        private function get _turnPts():uint {
            return __turnPts;
        }

        /**
         * @private
         */
        private function set _turnPts(value:uint):void {
            __turnPts = value;
            _ptsThisTurnText.text = String(value);

            if ( _isPlayer2 && __p2Score + value >= _maxScore ) {
                _ptsThisTurnText.text = "0";
                _p2Score += value;
            }
            else if ( ! _isPlayer2 && __p1Score + value >= _maxScore ) {
                _ptsThisTurnText.text = "0";
                _p1Score += value;
            }
        }

        /**
         * Function called when the "Roll dice" button is clicked.
         *
         * @private
         */
        private function _rollButtonClick(e:MouseEvent):void {
            var roll:uint = _rollDice();

            if ( roll == 1 ) {
                if ( _isPlayer2 ) {
                    _currentPlayerText.text = _name1 + "'s turn";
                    _statusText.text = _name2 + " rolls 1 and loses " + __turnPts + " points. " + _name1 + "'s turn now.";
                }
                else {
                    _currentPlayerText.text = _name2 + "'s turn";
                    _statusText.text = _name1 + " rolls 1 and loses " + __turnPts + " points. " + _name2 + "'s turn now.";
                }

                _isPlayer2 = ! _isPlayer2;
                _turnPts = 0;
            }
            else {
                _turnPts += roll;
                _statusText.text = "";
            }
        }

        /**
         * Function called when the "Hold" button is clicked.
         *
         * @private
         */
        private function _holdButtonClick(e:MouseEvent):void {
            if ( _isPlayer2 ) {
                _currentPlayerText.text = _name1 + "'s turn";
                _statusText.text = _name2 + " holds and wins " + __turnPts + " points. " + _name1 + "'s turn now.";
                _p2Score += __turnPts;
            }
            else {
                _currentPlayerText.text = _name2 + "'s turn";
                _statusText.text = _name1 + " holds and wins " + __turnPts + " points. " + _name2 + "'s turn now.";
                _p1Score += __turnPts;
            }

            _dice.visible = false;
            _turnPts = 0;
            _isPlayer2 = ! _isPlayer2;
        }

    }

}
