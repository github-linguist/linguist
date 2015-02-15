function compose(f:Function, g:Function):Function {
	return function(x:Object) {return f(g(x));};
}
function test() {
	trace(compose(Math.atan, Math.tan)(0.5));
}
