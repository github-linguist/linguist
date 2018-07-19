function shellSort (a) {
    for (var h = a.length; h = parseInt(h / 2);) {
        for (var i = h; i < a.length; i++) {
            var k = a[i];
            for (var j = i; j >= h && k < a[j - h]; j -= h)
                a[j] = a[j - h];
            a[j] = k;
        }
    }
    return a;
}

var a = [];
var n = location.href.match(/\?(\d+)|$/)[1] || 10;
for (var i = 0; i < n; i++)
    a.push(parseInt(Math.random() * 100));
shellSort(a);
document.write(a.join(" "));
