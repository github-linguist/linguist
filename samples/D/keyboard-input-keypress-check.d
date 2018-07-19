extern (C) {
    void _STI_conio();
    void _STD_conio();
    int kbhit();
    int getch();
}

void main() {
    _STI_conio();

    char c;
    if (kbhit())
        c = cast(char)getch();

    _STD_conio();
}
