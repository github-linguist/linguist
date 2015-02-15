function non_continuous_subsequences(ary) {
    var non_continuous = new Array();
    for (var i = 0; i < ary.length; i++) {
        if (! is_array_continuous(ary[i])) {
            non_continuous.push(ary[i]);
        }
    }
    return non_continuous;
}

function is_array_continuous(ary) {
    if (ary.length < 2)
        return true;
    for (var j = 1; j < ary.length; j++) {
        if (ary[j] - ary[j-1] != 1) {
            return false;
        }
    }
    return true;
}

load('json2.js'); /* http://www.json.org/js.html */

print(JSON.stringify( non_continuous_subsequences( powerset([1,2,3,4]))));
