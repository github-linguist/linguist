function knuth_shuffle(a) {
    var n = a.length,
        r,
        temp;
    while (n > 1) {
        r = Math.floor(n * Math.random());
        n -= 1;
        temp = a[n];
        a[n] = a[r];
        a[r] = temp;
    }
    return a;
}

var res, i, key;

res = {
    '1,2,3': 0, '1,3,2': 0,
    '2,1,3': 0, '2,3,1': 0,
    '3,1,2': 0, '3,2,1': 0
};

for (i = 0; i < 100000; i++) {
    res[knuth_shuffle([1,2,3]).join(',')] += 1;
}
for (key in res) {
    print(key + "\t" + res[key]);
}
