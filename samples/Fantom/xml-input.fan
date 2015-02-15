using xml

class XmlInput
{
  public static Void main ()
  {
    // create the XML parser
    parser := XParser(File("sample-xml.xml".toUri).in)
    // parse the document, creating an XML document
    XDoc doc := parser.parseDoc
    // walk through each child element from the root of the document
    doc.root.elems.each |elem|
    {
      // printing the Name attribute of all Students
      if (elem.name == "Student") { echo (elem.get("Name")) }
    }
  }
}
