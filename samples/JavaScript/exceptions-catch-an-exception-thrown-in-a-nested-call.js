function U() {}
U.prototype.toString = function(){return this.className;}

function U0() {
    this.className = arguments.callee.name;
}
U0.prototype = new U();

function U1() {
    this.className = arguments.callee.name;
}
U1.prototype = new U();

function foo() {
    for (var i = 1; i <= 2; i++) {
        try {
            bar();
        }
        catch(e if e instanceof U0) {
            print("caught exception " + e);
        }
    }
}

function bar() {
    baz();
}

function baz() {
    // during the first call, redefine the function for subsequent calls
    baz = function() {throw(new U1());}
    throw(new U0());
}

foo();
