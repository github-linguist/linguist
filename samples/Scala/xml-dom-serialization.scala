val xml = <root><element>Some text here</element></root>
scala.xml.XML.save(filename="output.xml", node=xml, enc="UTF-8", xmlDecl=true, doctype=null)
