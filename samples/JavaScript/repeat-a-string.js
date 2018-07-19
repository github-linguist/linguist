String.prototype.repeat = function(n) {
    return new Array(1 + n).join(this);
}

alert("ha".repeat(5));  // hahahahaha
