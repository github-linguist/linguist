require 'rexml/document'
include REXML

doc = Document.new(File.new("sample.xml"))
# or
# doc = Document.new(xml_string)

# without using xpath
doc.each_recursive do |node|
  puts node.attributes["Name"] if node.name == "Student"
end

# using xpath
doc.each_element("*/Student") {|node| puts node.attributes["Name"]}
