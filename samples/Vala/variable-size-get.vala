void main(){
    ulong x = sizeof(int); // returns # of bytes used to store an int
    // is returned as a ulong, but could be typecasted to int with: int x = (int) sizeof(int)
    stdout.printf("%lu\n", x);
}
