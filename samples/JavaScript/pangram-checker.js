function is_pangram(str) {
    var s = str.toLowerCase();
    // sorted by frequency ascending (http://en.wikipedia.org/wiki/Letter_frequency)
    var letters = "zqxjkvbpygfwmucldrhsnioate";
    for (var i = 0; i < 26; i++)
        if (s.indexOf(letters.charAt(i)) == -1)
            return false;
    return true;
}

print(is_pangram("is this a pangram"));  // false
print(is_pangram("The quick brown fox jumps over the lazy dog"));  // true
