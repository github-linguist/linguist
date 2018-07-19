final random = new Random()

while (true) {
    def random1 = random.nextInt(20)
    print random1
    if (random1 == 10) break
    print '     '
    println random.nextInt(20)
}
