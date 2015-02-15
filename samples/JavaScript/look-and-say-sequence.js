function lookandsay(str) {
    return str.replace(/(.)\1*/g, function(seq, p1){return seq.length.toString() + p1})
}

var num = "1";
for (var i = 10; i > 0; i--) {
    alert(num);
    num = lookandsay(num);
}
