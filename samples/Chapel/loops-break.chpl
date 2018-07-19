use Random;

var r = new RandomStream();
while true {
        var a = floor(r.getNext() * 20):int;
        writeln(a);
        if a == 10 then break;
        var b = floor(r.getNext() * 20):int;
        writeln(b);
}
delete r;
