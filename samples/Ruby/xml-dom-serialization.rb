require("rexml/document")
include REXML
(doc = Document.new) << XMLDecl.new
root = doc.add_element('root')
element = root.add_element('element')
element.add_text('Some text here')

# save to a string
# (the first argument to write() needs an object that understands "<<")
serialized = String.new
doc.write(serialized, 4)
puts serialized
