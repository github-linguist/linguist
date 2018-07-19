def isPangram(sentence :String) {
    return ("abcdefghijklmnopqrstuvwxyz".asSet() &! sentence.toLowerCase().asSet()).size() == 0
}
