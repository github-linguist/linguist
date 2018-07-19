def shuffle(text) {
    def shuffled = (text as List)
    for (sourceIndex in 0..<text.size()) {
        for (destinationIndex in 0..<text.size()) {
                if (shuffled[sourceIndex] != shuffled[destinationIndex] && shuffled[sourceIndex] != text[destinationIndex] && shuffled[destinationIndex] != text[sourceIndex]) {
                    char tmp = shuffled[sourceIndex];
                    shuffled[sourceIndex] = shuffled[destinationIndex];
                    shuffled[destinationIndex] = tmp;
                    break;
                }
        }
    }
    [original: text, shuffled: shuffled.join(""), score: score(text, shuffled)]
}

def score(original, shuffled) {
    int score = 0
    original.eachWithIndex { character, index ->
        if (character == shuffled[index]) {
            score++
        }
    }
    score
}

["abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"].each { text ->
    def result = shuffle(text)
    println "${result.original}, ${result.shuffled}, (${result.score})"
}
