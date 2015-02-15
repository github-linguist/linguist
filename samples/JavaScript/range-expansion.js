#!/usr/bin/env js

function main() {
    print(rangeExpand('-6,-3--1,3-5,7-11,14,15,17-20'));
}

function rangeExpand(rangeExpr) {

    function getFactors(term) {
        var matches = term.match(/(-?[0-9]+)-(-?[0-9]+)/);
        if (!matches) return {first:Number(term)};
        return {first:Number(matches[1]), last:Number(matches[2])};
    }

    function expandTerm(term) {
        var factors = getFactors(term);
        if (factors.length < 2) return [factors.first];
        var range = [];
        for (var n = factors.first; n <= factors.last;  n++) {
            range.push(n);
        }
        return range;
    }

    var result = [];
    var terms = rangeExpr.split(/,/);
    for (var t in terms) {
        result = result.concat(expandTerm(terms[t]));
    }

    return result;
}

main();
