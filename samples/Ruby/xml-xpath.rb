#Example taken from the REXML tutorial (http://www.germane-software.com/software/rexml/docs/tutorial.html)
require "rexml/document"
include REXML
#create the REXML Document from the string (%q is Ruby's multiline string, everything between the two @-characters is the string)
doc = Document.new(
        %q@<inventory title="OmniCorp Store #45x10^3">
             ...
           </inventory>
          @
                          )
# The invisibility cream is the first <item>
invisibility = XPath.first( doc, "//item" )
# Prints out all of the prices
XPath.each( doc, "//price") { |element| puts element.text }
# Gets an array of all of the "name" elements in the document.
names = XPath.match( doc, "//name" )
