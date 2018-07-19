//Throw an error if a non-number argument is used. (typeof evaluates to
// "number" for both integers and reals)
function checkType(obj:Object):void {
    if(typeof obj != "number")
	throw new ArgumentError("Expected integer or float argument. Recieved " + typeof obj);
}
function accumulator(sum:Object):Function {
    checkType(sum);
    return function(n:Object):Object {checkType(n); return sum += n};
}
var acc:Function=accumulator(2);
trace(acc(10));
trace(acc(4));
trace(acc("123")); //This causes an ArgumentError to be thrown.
