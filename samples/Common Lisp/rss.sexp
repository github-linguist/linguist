
(:TURTLE

 (:@PREFIX "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
 (:@PREFIX "owl:" "<http://www.w3.org/2002/07/owl#>")
 (:@PREFIX "dc:" "<http://purl.org/dc/elements/1.1/>")
 (:@PREFIX "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
 (:@PREFIX "rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")

 (:TRIPLES (:URIREF "<http://purl.org/rss/1.0/channel>")
 
   (:PREDICATE-OBJECT-LIST
     (:URIREF #1="<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>")
     (:OBJECTS
       (:QNAME "rdfs:Class")))
       
   (:PREDICATE-OBJECT-LIST
     (:QNAME "rdfs:comment")
     (:OBJECTS
       (:STRING "An RSS information channel.")))
       
   (:PREDICATE-OBJECT-LIST
     (:QNAME "rdfs:isDefinedBy")
     (:OBJECTS
       (:URIREF "<http://purl.org/rss/1.0/>")))
       
   (:PREDICATE-OBJECT-LIST
     (:QNAME "rdfs:label")
     (:OBJECTS
       (:STRING "Channel"))))
       
 (:TRIPLES (:URIREF "<http://purl.org/rss/1.0/description>")
  
   (:PREDICATE-OBJECT-LIST
     (:URIREF #1#)
     (:OBJECTS
       (:QNAME "rdf:Property")))
      
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:comment")
   (:OBJECTS (:STRING "A short text description of the subject.")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:isDefinedBy")
   (:OBJECTS (:URIREF "<http://purl.org/rss/1.0/>")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:label") (:OBJECTS (:STRING "Description")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:subPropertyOf") (:OBJECTS (:QNAME "dc:description"))))
 (:TRIPLES (:URIREF "<http://purl.org/rss/1.0/image>")
  (:PREDICATE-OBJECT-LIST (:URIREF #1#) (:OBJECTS (:QNAME "rdfs:Class")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:comment") (:OBJECTS (:STRING "An RSS image.")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:isDefinedBy")
   (:OBJECTS (:URIREF "<http://purl.org/rss/1.0/>")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:label") (:OBJECTS (:STRING "Image"))))
 (:TRIPLES (:URIREF "<http://purl.org/rss/1.0/item>")
  (:PREDICATE-OBJECT-LIST (:URIREF #1#) (:OBJECTS (:QNAME "rdfs:Class")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:comment") (:OBJECTS (:STRING "An RSS item.")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:isDefinedBy")
   (:OBJECTS (:URIREF "<http://purl.org/rss/1.0/>")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:label") (:OBJECTS (:STRING "Item"))))
 (:TRIPLES (:URIREF "<http://purl.org/rss/1.0/items>")
  (:PREDICATE-OBJECT-LIST (:URIREF #1#) (:OBJECTS (:QNAME "rdf:Property")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:comment")
   (:OBJECTS
    (:STRING "Points to a list of rss:item elements that are members of the subject channel.")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:isDefinedBy")
   (:OBJECTS (:URIREF "<http://purl.org/rss/1.0/>")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:label") (:OBJECTS (:STRING "Items"))))
 (:TRIPLES (:URIREF "<http://purl.org/rss/1.0/link>")
  (:PREDICATE-OBJECT-LIST (:URIREF #1#) (:OBJECTS (:QNAME "rdf:Property")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:comment")
   (:OBJECTS (:STRING "The URL to which an HTML rendering of the subject will link.")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:isDefinedBy")
   (:OBJECTS (:URIREF "<http://purl.org/rss/1.0/>")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:label") (:OBJECTS (:STRING "Link")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:subPropertyOf") (:OBJECTS (:QNAME "dc:identifier"))))
 (:TRIPLES (:URIREF "<http://purl.org/rss/1.0/name>")
  (:PREDICATE-OBJECT-LIST (:URIREF #1#) (:OBJECTS (:QNAME "rdf:Property")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:comment")
   (:OBJECTS (:STRING "The text input field's (variable) name.")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:isDefinedBy")
   (:OBJECTS (:URIREF "<http://purl.org/rss/1.0/>")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:label") (:OBJECTS (:STRING "Name"))))
 (:TRIPLES (:URIREF "<http://purl.org/rss/1.0/textinput>")
  (:PREDICATE-OBJECT-LIST (:URIREF #1#) (:OBJECTS (:QNAME "rdfs:Class")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:comment") (:OBJECTS (:STRING "An RSS text input.")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:isDefinedBy")
   (:OBJECTS (:URIREF "<http://purl.org/rss/1.0/>")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:label") (:OBJECTS (:STRING "Text Input"))))
 (:TRIPLES (:URIREF "<http://purl.org/rss/1.0/title>")
  (:PREDICATE-OBJECT-LIST (:URIREF #1#) (:OBJECTS (:QNAME "rdf:Property")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:comment")
   (:OBJECTS (:STRING "A descriptive title for the channel.")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:isDefinedBy")
   (:OBJECTS (:URIREF "<http://purl.org/rss/1.0/>")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:label") (:OBJECTS (:STRING "Title")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:subPropertyOf") (:OBJECTS (:QNAME "dc:title"))))
 (:TRIPLES (:URIREF "<http://purl.org/rss/1.0/url>")
  (:PREDICATE-OBJECT-LIST (:URIREF #1#) (:OBJECTS (:QNAME "rdf:Property")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:comment")
   (:OBJECTS
    (:STRING
     "The URL of the image to used in the 'src' attribute of the channel's image tag when rendered as HTML.")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:isDefinedBy")
   (:OBJECTS (:URIREF "<http://purl.org/rss/1.0/>")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:label") (:OBJECTS (:STRING "URL")))
  (:PREDICATE-OBJECT-LIST (:QNAME "rdfs:subPropertyOf") (:OBJECTS (:QNAME "dc:identifier")))))
