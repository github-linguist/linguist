function mode(ary) {
    var counter = {};
    var mode = [];
    var max = 0;
    for (var i in ary) {
        if (!(ary[i] in counter))
            counter[ary[i]] = 0;
        counter[ary[i]]++;

        if (counter[ary[i]] == max)
            mode.push(ary[i]);
        else if (counter[ary[i]] > max) {
            max = counter[ary[i]];
            mode = [ary[i]];
        }
    }
    return mode;
}

mode([1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17]);  // [6]
mode([1, 2, 4, 4, 1]);  // [1,4]
