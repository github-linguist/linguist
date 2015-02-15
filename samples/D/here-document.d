import std.stdio, std.string;

void main() {
    // Delimited strings: a 'q' followed by double quotes and an
    // opening and closing delimiter of choice:

    q"[a string that you "don't" have to escape]"
    .writeln;

    // If the delimiter is an identifier, the identifier must be
    // immediately followed by a newline, and the matching delimiter
    // is the same identifier starting at the beginning of the line:

    q"EOS
    This
    is a multi-line
    heredoc string
EOS".outdent.writeln;

    // std.string.outdent is used to remove the four spaces indent.
}
