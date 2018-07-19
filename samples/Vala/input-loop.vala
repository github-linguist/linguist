int main() {
        string? s;
        while((s = stdin.read_line()) != null) {
                stdout.printf("%s\n", s);
        }
        return 0;
}
