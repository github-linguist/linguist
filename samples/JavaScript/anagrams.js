var fs = require('fs');

var anas = {};
var words = fs.readFileSync('unixdict.txt', 'UTF-8').split('\n');
var max = 0;

for (var w in words) {
    var key = words[w].split('').sort().join('');
    if (!(key in anas)) {
        anas[key] = [];
    }
    var count = anas[key].push(words[w]);
    max = Math.max(count, max);
}

for (var a in anas) {
    if (anas[a].length === max) {
        console.log(anas[a]);
    }
}
