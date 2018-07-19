function Singleton() {
	if(Singleton._instance) return Singleton._instance;
	this.set("");
	Singleton._instance = this;
}

Singleton.prototype.set = function(msg) { this.msg = msg; }
Singleton.prototype.append = function(msg) { this.msg += msg; }
Singleton.prototype.get = function() { return this.msg; }


var a = new Singleton();
var b = new Singleton();
var c = new Singleton();

a.set("Hello");
b.append(" World");
c.append("!!!");

document.write( (new Singleton()).get() );
