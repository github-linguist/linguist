module xmltest ;

import std.stdio ;
import std.xml ;

void main() {
  auto doc = new Document("root") ;
//doc.prolog = q"/<?xml version="1.0"?>/" ; // default
  doc ~= new Element("element", "Some text here") ;
  writefln(doc) ;
// output: <?xml version="1.0"?><root><element>Some text here</element></root>
}
