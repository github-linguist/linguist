def hq9plus(code: String) : String = {
    var out = ""
    var acc = 0

    def bottle(num: Int) : Unit = {
        if (num > 1) {
            out += num + " bottles of beer on the wall, " + num + " bottles of beer.\n"
            out += "Take one down and pass it around, " + (num - 1) + " bottle"

            if (num > 2) out += "s"

            out += " of beer.\n\n"
            bottle(num - 1)
        }
        else {
            out += "1 bottle of beer on the wall, 1 bottle of beer.\n" +
                "Take one down and pass it around, no more bottles of beer on the wall.\n\n" +
                "No more bottles of beer on the wall, no more bottles of beer.\n" +
                "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
        }
    }

    def handle(char: Char) = char match {
        case 'H' => out += "Hello world!\n"
        case 'Q' => out += code + "\n"
        case '+' => acc += 1
        case '9' => bottle(99)
    }

    code.toList foreach handle
    out
}

println(hq9plus("HQ9+"))
