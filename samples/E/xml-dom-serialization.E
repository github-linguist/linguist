def document := <unsafe:javax.xml.parsers.makeDocumentBuilderFactory> \
                  .newInstance() \
                  .newDocumentBuilder() \
                  .getDOMImplementation() \
                  .createDocument(null, "root", null)
def root := document.getDocumentElement()
root.appendChild(
  def element := document.createElement("element"))
element.appendChild(
  document.createTextNode("Some text here"))
println(document.saveXML(root))
