#!/usr/bin/env js

function main() {
    print('Floyd 5:');
    floyd(5);
    print('\nFloyd 14:');
    floyd(14);
}


function padLeft(s, w) {
    for (s = String(s); s.length < w; s = ' ' + s);
    return s;
}


function floyd(nRows) {
    var lowerLeft = nRows * (nRows - 1) / 2 + 1;
    var lowerRight = nRows * (nRows + 1) / 2;

    var colWidths = [];
    for (var col = lowerLeft; col <= lowerRight; col++) {
        colWidths.push(String(col).length);
    }

    var  num = 1;
    for (var row = 0; row < nRows; row++) {
        var line = [];
        for (var col = 0; col <= row; col++, num++) {
            line.push(padLeft(num, colWidths[col]));
        }
        print(line.join(' '));
    }
}

main();
