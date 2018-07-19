// Derived from AA3D - ASCII art stereogram generator
// by Jan Hubicka and Thomas Marsh
// (GNU General Public License)
// http://aa-project.sourceforge.net/aa3d/
// http://en.wikipedia.org/wiki/ASCII_stereogram

import std.stdio, std.string, std.random;

immutable image = "
                  111111111111111
                  1111111111111111
                   11111       1111
                   11111        1111
                   11111        1111
                   11111        1111
                   11111        1111
                   11111       1111
                  1111111111111111
                  111111111111111

";

void main() {
    enum int width = 50;
    immutable text = "DLanguage";
    enum int skip = 12;

    char[65536 / 2] data;

    foreach (y, row; image.splitLines()) {
        immutable int shift = uniform(0, int.max);
        bool l = false;

        foreach (x; 0 .. width) {
            int s;
            if (!l && x > skip) {
                s = (x < row.length) ? row[x] : '\n';
                if (s == ' ') {
                    s = 0;
                } else if (s == '\n') {
                    s = 0;
                    l = true;
                } else if (s >= '0' && s <= '9') {
                    s = '0' - s;
                } else
                    s = -2;
            } else
                s = 0;

            s += skip;
            s = x - s;
            s = (s < 0) ? text[(x + shift) % text.length] : data[s];
            data[x] = cast(char)s;
            write(data[x]);
        }

        writeln();
    }
}
