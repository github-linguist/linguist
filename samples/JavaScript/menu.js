function select(question, choices) {
    var prompt = "";
    for (var i in choices)
        prompt += i + ". " + choices[i] + "\n";
    prompt += question;

    var input;
    while (1) {
        WScript.Echo(prompt);
        input = parseInt( WScript.StdIn.readLine() );
        if (0 <= input && input < choices.length)
            break;
        WScript.Echo("\nTry again.");
    }
    return input;
}

var choices = ['fee fie', 'huff and puff', 'mirror mirror', 'tick tock'];
var choice = select("Which is from the three pigs?", choices);
WScript.Echo("you selected: " + choice + " -> " + choices[choice]);
