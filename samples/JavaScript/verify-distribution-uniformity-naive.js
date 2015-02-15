function distcheck(random_func, times, opts) {
    if (opts === undefined) opts = {}
    opts['delta'] = opts['delta'] || 2;

    var count = {}, vals = [];
    for (var i = 0; i < times; i++) {
        var val = random_func();
        if (! has_property(count, val)) {
            count[val] = 1;
            vals.push(val);
        }
        else
            count[val] ++;
    }
    vals.sort(function(a,b) {return a-b});

    var target = times / vals.length;
    var tolerance = target * opts['delta'] / 100;

    for (var i = 0; i < vals.length; i++) {
        var val = vals[i];
        if (Math.abs(count[val] - target) > tolerance)
            throw "distribution potentially skewed for " + val +
                  ": expected result around " + target + ", got " +count[val];
        else
            print(val + "\t" + count[val]);
    }
}

function has_property(obj, propname) {
    return typeof(obj[propname]) == "undefined" ? false : true;
}

try {
    distcheck(function() {return Math.floor(10 * Math.random())}, 100000);
    print();
    distcheck(function() {return (Math.random() > 0.95 ? 1 : 0)}, 100000);
} catch (e) {
    print(e);
}
