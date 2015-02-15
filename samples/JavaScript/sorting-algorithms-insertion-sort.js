function insertionSort (a) {
    for (var i = 0; i < a.length; i++) {
        var k = a[i];
        for (var j = i; j > 0 && k < a[j - 1]; j--)
            a[j] = a[j - 1];
        a[j] = k;
    }
    return a;
}

var a = [4, 65, 2, -31, 0, 99, 83, 782, 1];
insertionSort(a);
document.write(a.join(" "));
