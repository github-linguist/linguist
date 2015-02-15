#!/usr/bin/env js

function main() {
    var nums = getNumbers(11);
    nums.reverse();
    for (var i in  nums) {
        pardoKnuth(nums[i], fn, 400);
    }
}

function pardoKnuth(n, f, max) {
    var res = f(n);
    putstr('f(' + String(n) + ')');
    if (res > max) {
        print(' is too large');
    } else {
        print(' = ' + String(res));
    }
}

function fn(x) {
    return Math.pow(Math.abs(x), 0.5) + 5 * Math.pow(x, 3);
}

function getNumbers(n) {
    var nums = [];
    print('Enter', n, 'numbers.');
    for (var i = 1; i <= n; i++) {
        putstr('   ' + i + ': ');
        var num = readline();
        nums.push(Number(num));
    }
    return nums;
}

main();
