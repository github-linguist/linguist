function Complex(r, i) {
	this.r = r;
	this.i = i;
}

Complex.add = function() {
	var num = arguments[0];
	
	for(var i = 1, ilim = arguments.length; i < ilim; i += 1){
		num.r += arguments[i].r;
		num.i += arguments[i].i;
	}
	
	return num;
}

Complex.multiply = function() {
	var num = arguments[0];
	
	for(var i = 1, ilim = arguments.length; i < ilim; i += 1){
		num.r = (num.r * arguments[i].r) - (num.i * arguments[i].i);
		num.i = (num.i * arguments[i].r) - (num.r * arguments[i].i);
	}
	
	return num;
}

Complex.negate = function (z) {
	return new Complex(-1*z.r, -1*z.i);
}

Complex.invert = function(z) {
	var denom = Math.pow(z.r,2) + Math.pow(z.i,2);
	return new Complex(z.r/denom, -1*z.i/denom);
}

Complex.conjugate = function(z) {
	return new Complex(z.r, -1*z.i);
}

// BONUSES!


Complex.prototype.toString = function() {
	return this.r === 0 && this.i === 0
          ? "0"
          : (this.r !== 0 ? this.r : "")
          + ((this.r !== 0 || this.i < 0) && this.i !== 0
              ? (this.i > 0 ? "+" : "-")
              : "" ) + ( this.i !== 0 ? Math.abs(this.i) + "i" : "" );
}

Complex.prototype.getMod = function() {
	return Math.sqrt( Math.pow(this.r,2) , Math.pow(this.i,2) )
}
