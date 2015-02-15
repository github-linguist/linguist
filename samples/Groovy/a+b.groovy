def abAdder = {
    def reader = new Scanner(System.in)
    def a = reader.nextInt();
    def b = reader.nextInt();
    assert (-1000..1000).containsAll([a,b]) : "both numbers must be between -1000 and 1000 (inclusive)"
    a + b
}
abAdder()
