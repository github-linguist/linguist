enum Rule {
    r01( 1, { r()*.num == (1..12) }),
    r02( 2, { r(7..12).count { it.truth } == 3 }),
    r03( 3, { r(2..12, 2).count { it.truth } == 2 }),
    r04( 4, { r(5).truth ? r(6).truth && r(7).truth : true }),
    r05( 5, { r(2..4).count { it.truth } == 0 }),
    r06( 6, { r(1..11, 2).count { it.truth } == 4 }),
    r07( 7, { r(2).truth != r(3).truth }),
    r08( 8, { r(7).truth ? r(5).truth && r(6).truth : true }),
    r09( 9, { r(1..6).count { it.truth } == 3 }),
    r10(10, { r(11).truth && r(12).truth }),
    r11(11, { r(7..9).count { it.truth } == 1 }),
    r12(12, { r(1..11).count { it.truth } == 4 });

    final int num
    final Closure statement
    boolean truth

    static final List<Rule> rules = [ null, r01, r02, r03, r04, r05, r06, r07, r08, r09, r10, r11, r12]

    private Rule(num, statement) {
        this.num = num
        this.statement = statement
    }

    public static Rule       r(int index) { rules[index] }
    public static List<Rule> r() { rules[1..12] }
    public static List<Rule> r(List<Integer> indices) { rules[indices] }
    public static List<Rule> r(IntRange indices) { rules[indices] }
    public static List<Rule> r(IntRange indices, int step) { r(indices.step(step)) }

    public static void setAllTruth(int bits) {
        (1..12).each { r(it).truth = !(bits & (1 << (12 - it))) }
    }

    public static void evaluate() {
        def nearMisses = [:]
        (0..<(2**12)).each { i ->
            setAllTruth(i)
            def truthCandidates = r().findAll { it.truth }
            def truthMatchCount = r().count { it.statement() == it.truth }
            if (truthMatchCount == 12) {
                println ">Solution< ${truthCandidates*.num}"
            } else if (truthMatchCount == 11) {
                def miss = (1..12).find { r(it).statement() != r(it).truth }
                nearMisses << [(truthCandidates): miss]
            }
        }
        nearMisses.each { truths, miss ->
            printf ("Near Miss: %-21s (failed %2d)\n", "${truths*.num}", miss)
        }
    }
}

Rule.evaluate()
