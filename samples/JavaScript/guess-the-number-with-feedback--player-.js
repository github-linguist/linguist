#!/usr/bin/env js

var DONE = RIGHT = 0, HIGH = 1, LOW = -1;

function main() {
    showInstructions();
    while (guess(1, 100) !== DONE);
}

function guess(low, high) {
    if (low > high) {
        print("I can't guess it. Perhaps you changed your number.");
        return DONE;
    }

    var g = Math.floor((low + high) / 2);
    var result = getResult(g);
    switch (result) {
        case RIGHT:
            return DONE;
        case LOW:
            return guess(g + 1, high);
        case HIGH:
            return guess(low, g - 1);
    }
}

function getResult(g) {
    while(true) {
        putstr('Is it ' + g + '? ');
        var ans = readline().toUpperCase().replace(/^\s+/, '') + ' ';
        switch (ans[0]) {
            case 'R':
                print('I got it! Thanks for the game.');
                return RIGHT;
            case 'L':
                return LOW;
            case 'H':
                return HIGH;
            default:
                print('Please tell me if I am "high", "low" or "right".');
        }
    }
}

function showInstructions() {
    print('Think of a number between 1 and 100 and I will try to guess it.');
    print('After I guess, type "low", "high" or "right", and then press enter.');
    putstr("When you've thought of a number press enter.");
    readline();
}

main();
