final random = new Random()
final input = new Scanner(System.in)


def evaluate = { expr ->
    if (expr == 'QUIT') {
        return 'QUIT'
    } else {
        try { Eval.me(expr.replaceAll(/(\d)/, '$1.0')) }
        catch (e) { 'syntax error' }
    }
}


def readGuess = { digits ->
    while (true) {
        print "Enter your guess using ${digits} (q to quit): "
        def expr = input.nextLine()

        switch (expr) {
            case ~/^[qQ].*/:
                return 'QUIT'

            case ~/.*[^\d\s\+\*\/\(\)-].*/:
                def badChars = expr.replaceAll(~/[\d\s\+\*\/\(\)-]/, '')
                println "invalid characters in input: ${(badChars as List) as Set}"
                break

            case { (it.replaceAll(~/\D/, '') as List).sort() != ([]+digits).sort() }:
                println '''you didn't use the right digits'''
                break

            case ~/.*\d\d.*/:
                println 'no multi-digit numbers allowed'
                break

            default:
                return expr
        }
    }
}


def digits = (1..4).collect { (random.nextInt(9) + 1) as String }

while (true) {
    def guess = readGuess(digits)
    def result = evaluate(guess)

    switch (result) {
        case 'QUIT':
            println 'Awwww. Maybe next time?'
            return

        case 24:
            println 'Yes! You got it.'
            return

        case 'syntax error':
            println "A ${result} was found in ${guess}"
            break

        default:
            println "Nope: ${guess} == ${result}, not 24"
            println 'One more try, then?'
    }
}
