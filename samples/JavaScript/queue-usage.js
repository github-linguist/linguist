var f = new Array();
print(f.length);
f.push(1,2);         // can take multiple arguments
f.push(3);
f.shift();
f.shift();
print(f.length);
print(f.shift())
print(f.length == 0);
print(f.shift());
