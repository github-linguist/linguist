// Krypton tree-walking evaluator
// Source: https://github.com/krypton-lang/krypton/blob/main/kompiler/eval.k
// License: MIT

go eval {

    func eval(ast) {
        if startsWith(ast, "(num ") {
            let n = substring(ast, 5, len(ast) - 1)
            emit toInt(n)
        }

        if startsWith(ast, "(add ") {
            let a = extract(ast, 5)
            let b = extract(ast, findSecond(ast))
            emit eval(a) + eval(b)
        }

        if startsWith(ast, "(sub ") {
            let a = extract(ast, 5)
            let b = extract(ast, findSecond(ast))
            emit eval(a) - eval(b)
        }

        if startsWith(ast, "(mul ") {
            let a = extract(ast, 5)
            let b = extract(ast, findSecond(ast))
            emit eval(a) * eval(b)
        }

        if startsWith(ast, "(div ") {
            let a = extract(ast, 5)
            let b = extract(ast, findSecond(ast))
            emit eval(a) / eval(b)
        }

        emit 0
    }
}
