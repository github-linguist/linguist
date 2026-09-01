head(custom) {
    fn smallest(I64 a, I64 b) {
        if (a <= b) {
            return a
        }
        return b
    }
    fn main() {
        I64 result = smallest(9859285082058292, 9492389428498249829)
        pin("the smallest is:%I64\n", result)
    }
}
