import std.file, std.conv, std.string;

void main() {
    auto x = [1.0, 2, 3, 1e11];
    auto y = [1.0, 1.4142135623730951,
              1.7320508075688772, 316227.76601683791];
    int xPrecision = 3,
        yPrecision = 5;

    string tmp;
    foreach (i, fx; x)
        tmp ~= format("%." ~ text(xPrecision) ~ "g      %." ~
                      text(yPrecision) ~ "g\r\n", fx, y[i]);

    write("float_array.txt", tmp);
}
