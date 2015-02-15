def notes = new File('./notes.txt')
if (args) {
    notes << "${new Date().format('YYYY-MM-dd HH:mm:ss')}\t${args.join(' ')}\n"
} else {
    println notes.text
}
