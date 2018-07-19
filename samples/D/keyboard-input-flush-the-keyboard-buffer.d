extern (C) {
    void _STI_conio();
    void _STD_conio();
    int kbhit();
    int getch();
}

void main() {
    void flushKB() {
        while (kbhit()) getch();
    }

    _STI_conio();

    flushKB();

    _STD_conio();
}
