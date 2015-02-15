DOM = XMLObject["Document"][{XMLObject["Declaration"]["Version" -> "1.0","Encoding" -> "utf-8"]},
XMLElement["root", {}, {XMLElement["element", {}, {"Some text here"}]}], {}];

ExportString[DOM, "XML", "AttributeQuoting" -> "\""]
