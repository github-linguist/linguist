var bases = [2, 8, 10, 16, 24];
for (var n = 0; n <= 33; n++) {
    var row = [];
    for (var i = 0; i < bases.length; i++)
        row.push( n.toString(bases[i]) );
    print(row.join(', '));
}
