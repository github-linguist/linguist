void main() {
    // Initialized:
    int a = 5;
    double b = 5.0;
    char c = 'f';
    int[] d = [1, 2, 3];

    // Default initialized:
    int aa; // set to 0
    double bb; // set to double.init, that is a NaN
    char cc; // set to 0xFF
    int[] dd; // set to null
    int[3] ee; // set to [0, 0, 0]

    // Undefined (contain garbage):
    int aaa = void;
    double[] bbb = void;
    int[3] eee = void;
}
