println("Downloading...")
when (def wordText := <http://www.puzzlers.org/pub/wordlists/unixdict.txt> <- getText()) -> {
    def words := wordText.split("\n")

    def storage := [].asMap().diverge()
    def anagramTable extends storage {
        to get(key) { return storage.fetch(key, fn { storage[key] := [].diverge() }) }
    }

    println("Grouping...")
    var largestGroupSeen := 0
    for word in words {
        def anagramGroup := anagramTable[word.sort()]
        anagramGroup.push(word)
        largestGroupSeen max= anagramGroup.size()
    }

    println("Selecting...")
    for _ => anagramGroup ? (anagramGroup.size() == mostSeen) in anagramTable {
        println(anagramGroup.snapshot())
    }
}
