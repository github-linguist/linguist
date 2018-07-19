import groovy.xml.MarkupBuilder
def writer = new StringWriter() << '<?xml version="1.0" ?>\n'
def xml = new MarkupBuilder(writer)
xml.root() {
    element('Some text here' )
}
println writer
