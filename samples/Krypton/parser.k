// Krypton recursive-descent parser
// Source: https://github.com/krypton-lang/krypton/blob/main/kompiler/parser.k
// License: MIT

go parser {

    func nextToken(tokens, i) {
        emit split(tokens, i)
    }

    func parseFactor(tokens, i) {
        let tok = nextToken(tokens, i)

        if startsWith(tok, "INT:") {
            let n = substring(tok, 4, len(tok))
            emit "(num " + n + ")," + (i + 1)
        }

        if tok == "LPAREN" {
            let pair = parseExpr(tokens, i + 1)
            let expr = split(pair, 0)
            let ni = toInt(split(pair, 1))
            emit expr + "," + (ni + 1)
        }

        emit "(num 0)," + (i + 1)
    }

    func parseTerm(tokens, i) {
        let pair = parseFactor(tokens, i)
        let left = split(pair, 0)
        let pos = toInt(split(pair, 1))

        while pos < count(tokens) {
            let tok = nextToken(tokens, pos)

            if tok == "STAR" {
                let pair2 = parseFactor(tokens, pos + 1)
                let right = split(pair2, 0)
                pos = toInt(split(pair2, 1))
                left = "(mul " + left + " " + right + ")"
            }
            else if tok == "SLASH" {
                let pair2 = parseFactor(tokens, pos + 1)
                let right = split(pair2, 0)
                pos = toInt(split(pair2, 1))
                left = "(div " + left + " " + right + ")"
            }
            else {
                break
            }
        }

        emit left + "," + pos
    }

    func parseExpr(tokens, i) {
        let pair = parseTerm(tokens, i)
        let left = split(pair, 0)
        let pos = toInt(split(pair, 1))

        while pos < count(tokens) {
            let tok = nextToken(tokens, pos)

            if tok == "PLUS" {
                let pair2 = parseTerm(tokens, pos + 1)
                let right = split(pair2, 0)
                pos = toInt(split(pair2, 1))
                left = "(add " + left + " " + right + ")"
            }
            else if tok == "MINUS" {
                let pair2 = parseTerm(tokens, pos + 1)
                let right = split(pair2, 0)
                pos = toInt(split(pair2, 1))
                left = "(sub " + left + " " + right + ")"
            }
            else {
                break
            }
        }

        emit left + "," + pos
    }

    func parse(tokens) {
        let pair = parseExpr(tokens, 0)
        emit split(pair, 0)
    }
}
