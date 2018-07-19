function gnomeSort(a) {
    function moveBack(i) {
        for( ; i > 0 && a[i-1] > a[i]; i--) {
            var t = a[i];
            a[i] = a[i-1];
            a[i-1] = t;
        }
    }
    for (var i = 1; i < a.length; i++) {
        if (a[i-1] > a[i]) moveBack(i);
    }
    return a;
}
