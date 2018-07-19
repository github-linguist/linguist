var cube:Function = function(x) {
  return Math.pow(x, 3);
};
var cuberoot:Function = function(x) {
  return Math.pow(x, 1/3);
};

function compose(f:Function, g:Function):Function {
	return function(x:Number) {return f(g(x));};
}
var functions:Array = [Math.cos, Math.tan, cube];
var inverse:Array = [Math.acos, Math.atan, cuberoot];

function test() {
	for (var i:uint = 0; i < functions.length; i++) {
        // Applying the composition to 0.5
	trace(compose(functions[i], inverse[i])(0.5));
	}
}

test();
