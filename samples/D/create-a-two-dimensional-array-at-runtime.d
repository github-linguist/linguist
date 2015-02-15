void main() {
    import std.stdio, std.conv, std.string;
    int nRow, nCol;

    write("Give me the numer of rows: ");
    try {
        nRow = readln.strip.to!int;
    } catch (StdioException) {
        nRow = 3;
        writeln;
    }

    write("Give me the numer of columns: ");
    try {
        nCol = readln.strip.to!int;
    } catch (StdioException) {
        nCol = 5;
        writeln;
    }

    auto array = new float[][](nRow, nCol);
    array[0][0] = 3.5;
    writeln("The number at place [0, 0] is ", array[0][0]);
}
