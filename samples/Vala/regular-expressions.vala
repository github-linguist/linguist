void main(){
    string sentence = "This is a sample sentence.";

    Regex a = new Regex("s[ai]mple"); // if using \n type expressions, use triple " for string literals as easy method to escape them

    if (a.match(sentence)){
        stdout.printf("\"%s\" is in \"%s\"!\n", a.get_pattern(), sentence);
    }

    string sentence_replacement = "cat";
    sentence = a.replace(sentence, sentence.length, 0, sentence_replacement);
    stdout.printf("Replaced sentence is: %s\n", sentence);
}
