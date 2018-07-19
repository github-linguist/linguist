def writer = new StringWriter()
def builder = new groovy.xml.MarkupBuilder(writer)
def names = ["April", "Tam O'Shanter", "Emily"]
def remarks = ["Bubbly: I'm > Tam and <= Emily", 'Burns: "When chapman billies leave the street ..."', "Short & shrift"]

builder.CharacterRemarks() {
    names.eachWithIndex() { n, i -> Character(name:n, remarks[i]) };
}

println writer.toString()
