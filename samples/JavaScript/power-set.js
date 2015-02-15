function powerset(ary) {
    var ps = [[]];
    for (var i=0; i < ary.length; i++) {
        for (var j = 0, len = ps.length; j < len; j++) {
            ps.push(ps[j].concat(ary[i]));
        }
    }
    return ps;
}

var res = powerset([1,2,3,4]);

load('json2.js');
print(JSON.stringify(res));
