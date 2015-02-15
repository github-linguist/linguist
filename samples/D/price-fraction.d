import std.stdio, std.range;

double priceRounder(in double price) pure nothrow
in {
    assert(price >= 0 && price <= 1.0);
} body {
    static immutable cin  = [.06, .11, .16, .21, .26, .31, .36, .41,
                             .46, .51, .56, .61, .66, .71, .76, .81,
                             .86, .91, .96, 1.01],
                     cout = [.10, .18, .26, .32, .38, .44, .50, .54,
                             .58, .62, .66, .70, .74, .78, .82, .86,
                             .90, .94, .98, 1.00];
    return cout[cin.assumeSorted.lowerBound(price).length];
}

void main() {
    foreach (const price; [0.7388727, 0.8593103, 0.826687, 0.3444635])
        price.priceRounder.writeln;
}
