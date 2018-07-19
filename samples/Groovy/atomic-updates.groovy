class Buckets {

    def cells = []
    final n

    Buckets(n, limit=1000, random=new Random()) {
        this.n = n
        (0..<n).each {
            cells << random.nextInt(limit)
        }
    }

    synchronized getAt(i) {
        cells[i]
    }

    synchronized transfer(from, to, amount) {
        assert from in (0..<n) && to in (0..<n)
        def cappedAmt = [cells[from], amount].min()
        cells[from] -= cappedAmt
        cells[to] += cappedAmt
    }

    synchronized String toString() { cells.toString() }
}

def random = new Random()

def buckets = new Buckets(5)

def makeCloser = { i, j ->
    synchronized(buckets) {
        def targetDiff = (buckets[i]-buckets[j]).intdiv(2)
        if (targetDiff < 0) {
            buckets.transfer(j, i, -targetDiff)
        } else {
            buckets.transfer(i, j, targetDiff)
        }
    }
}

def randomize = { i, j ->
    synchronized(buckets) {
        def targetLimit = buckets[i] + buckets[j]
        def targetI = random.nextInt(targetLimit + 1)
        if (targetI < buckets[i]) {
            buckets.transfer(i, j, buckets[i] - targetI)
        } else {
            buckets.transfer(j, i, targetI - buckets[i])
        }
    }
}

Thread.start {
    def start = System.currentTimeMillis()
    while (start + 10000 > System.currentTimeMillis()) {
        def i = random.nextInt(buckets.n)
        def j = random.nextInt(buckets.n)
        makeCloser(i, j)
    }
}

Thread.start {
    def start = System.currentTimeMillis()
    while (start + 10000 > System.currentTimeMillis()) {
        def i = random.nextInt(buckets.n)
        def j = random.nextInt(buckets.n)
        randomize(i, j)
    }
}

def start = System.currentTimeMillis()
while (start + 10000 > System.currentTimeMillis()) {
    synchronized(buckets) {
        def sum = buckets.cells.sum()
        println "${new Date()}: checksum: ${sum} buckets: ${buckets}"
    }
    Thread.sleep(500)
}
