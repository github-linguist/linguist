void main() {
    uint bottles = 99;
    do {
        print("%u bottles of beer on the wall.\n", bottles);
        print("%u bottles of beer!\n", bottles);
        print("Take one down, pass it around!\n");
        --bottles;
        if (bottles == 0) {
            print("No bottles");
        }
        else if (bottles == 1) {
            print("1 bottle");
        }
        else {
            print("%u bottles", bottles);
        }
        print(" of beer on the wall!\n\n");
    } while (bottles != 0);
}
