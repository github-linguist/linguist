extern (C) nothrow {
    void disp_open();
    void disp_move(int, int);
    void disp_eeop();
    void disp_close();
}

void main() {
    disp_open();
    disp_move(0, 0);
    disp_eeop();
    disp_close();
}
