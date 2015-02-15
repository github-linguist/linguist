function median(ary) {
    if (ary.length == 0)
        return null;
    ary.sort(function (a,b){return a - b})
    var mid = Math.floor(ary.length / 2);
    if ((ary.length % 2) == 1)  // length is odd
        return ary[mid];
    else
        return (ary[mid - 1] + ary[mid]) / 2;
}

median([]);   // null
median([5,3,4]);  // 4
median([5,4,2,3]);  // 3.5
median([3,4,1,-8.4,7.2,4,1,1.2]);  // 2.1
