import groovy.xml.MarkupBuilder

def createTable(columns, rowCount) {
    def writer = new StringWriter()
    new MarkupBuilder(writer).table(style: 'border:1px solid;text-align:center;') {
        tr {
            th()
            columns.each { title -> th(title)}
        }
        (1..rowCount).each { row ->
            tr {
                td(row)
                columns.each { td((Math.random() * 9999) as int ) }
            }
        }
    }
    writer.toString()
}

println createTable(['X', 'Y', 'Z'], 3)
