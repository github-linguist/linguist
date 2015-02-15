//create the text box
var textBox:TextField = new TextField();
addChild(textBox);

var text = "Hello, World! ";
var goingRight = true;

//modify the string and update it in the text box
function animate(e:Event)
{
	if(goingRight)
		text = text.slice(text.length-1,text.length) + text.slice(0, text.length - 1);
	else
		text = text.slice(1) + text.slice(0,1);
	textBox.text = text;
}

//event handler to perform the animation
textBox.addEventListener(Event.ENTER_FRAME, animate);
//event handler to register clicks
textBox.addEventListener(MouseEvent.MOUSE_DOWN, function(){goingRight = !goingRight;});
