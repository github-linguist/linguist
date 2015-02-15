def list = [25, 30, 1, 450, 3, 78]
def random = new Random();

(0..3).each {
    def i = random.nextInt(list.size())
    println "list[${i}] == ${list[i]}"
}
