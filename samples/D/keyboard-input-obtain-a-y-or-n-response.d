import std.stdio: stdout, write, writefln;

extern (C) nothrow {
    void _STI_conio();
    void _STD_conio();
    int kbhit();
    int getch();
}

void main() {
    _STI_conio();
    write("Enter Y or N: ");
    stdout.flush();

    int c;
    do {
        while(!kbhit()) {}
        c = getch();

        // Visual feedback for each keypress.
        write(cast(char)c);
        stdout.flush();
    } while(c != 'Y' && c != 'y' && c != 'N' && c != 'n');

    writefln("\nResponse: %c", cast(char)c);
    _STD_conio();
}
