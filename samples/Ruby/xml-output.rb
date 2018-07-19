require 'rexml/document'
include REXML

remarks = {
  %q(April)         => %q(Bubbly: I'm > Tam and <= Emily),
  %q(Tam O'Shanter) => %q(Burns: "When chapman billies leave the street ..."),
  %q(Emily)         => %q(Short & shrift),
}

doc = Document.new
root = doc.add_element("CharacterRemarks")

remarks.each do |name, remark|
  root.add_element("Character", {'Name' => name}).add_text(remark)
end

# output with indentation
doc.write($stdout, 2)
