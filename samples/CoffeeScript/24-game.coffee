tty = require 'tty'
tty.setRawMode true

buffer  = ""
numbers = []

for n in [0...4]
    numbers.push Math.max 1, Math.floor(Math.random() * 9)

console.log "You can use the numbers: #{numbers.join ' '}"

process.stdin.on 'keypress', (char, key) ->

    # accept operator
    if char and isNaN(char) and /[()*\/+-]/.test(char) and buffer.substr(-1) isnt char
        buffer += char
        process.stdout.write char
    # accept number
    else if !isNaN(+char) and (buffer == '' or isNaN(buffer.substr -1))
        buffer += char
        process.stdout.write char

    # check then evaluate expression
    if key?.name is 'enter'
        result = calculate()
        process.stdout.write '\n'
        if result and result is 24
            console.log " = 24! congratulations."
        else
            console.log "#{result}. nope."
        process.exit 0

    # quit
    if key?.name is 'escape' or (key?.name == 'c' and key.ctrl)
        process.exit 0

calculate = () ->

    if /[^\d\s()+*\/-]/.test buffer
        console.log "invalid characters"
        process.exit 1

    used = buffer.match(/\d/g)
    if used?.length != 4 or used.sort().join() != numbers.sort().join()
        console.log "you must use the 4 numbers provided"
        process.exit 1

    res = try eval buffer catch e
    return res or 'invalid expression'


# begin taking input
process.stdin.resume()
