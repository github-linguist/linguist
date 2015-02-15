def strdate(date) {
  return E.toString(<unsafe:java.util.makeDate>(date))
}

def test(type, file) {
    def t := file.lastModified()
    println(`The following $type called ${file.getPath()} ${
            if (t == 0) { "does not exist." } else { `was modified at ${strdate(t)}` }}`)
    println(`The following $type called ${file.getPath()} ${
            escape ne { file.setLastModified(timer.now(), ne); "was modified to current time." } catch _ { "does not exist." }}`)
    println(`The following $type called ${file.getPath()} ${
            escape ne { file.setLastModified(t, ne); "was modified to previous time." } catch _ { "does not exist." }}`)
}

test("file", <file:output.txt>)
test("directory", <file:docs>)
