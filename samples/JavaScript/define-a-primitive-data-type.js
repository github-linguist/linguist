function Num(n){
    n = Math.floor(n);
    if(isNaN(n))
        throw new TypeError("Not a Number");
    if(n < 1 || n > 10)
        throw new TypeError("Out of range");
    this._value = n;
}
Num.prototype.valueOf = function() { return this._value; }
Num.prototype.toString = function () { return this._value.toString();}

var w = new Num(3), x = new Num(4);

WScript.Echo(w + x); //7
WScript.Echo(x - w); //1
WScript.Echo(w * x); //12
WScript.Echo(w / x); //0.75
WScript.Echo(w < x); //true
WScript.Echo(x < w); //false

var y = new Num(0); //TypeError
var z = new Num(11); //TypeError
