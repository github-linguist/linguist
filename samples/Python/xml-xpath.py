# Python has basic xml parsing built in

from xml.dom import minidom

xmlfile = file("test3.xml") # load xml document from file
xmldoc = minidom.parse(xmlfile).documentElement # parse from file stream or...
xmldoc = minidom.parseString("<inventory title="OmniCorp Store #45x10^3">...</inventory>").documentElement # alternatively, parse a string
	
#  1st Task: Retrieve the first "item" element
i = xmldoc.getElementsByTagName("item") # get a list of all "item" tags
firstItemElement = i[0] # get the first element

# 2nd task: Perform an action on each "price" element (print it out)
for j in xmldoc.getElementsByTagName("price"): # get a list of all "price" tags
	print j.childNodes[0].data # XML Element . TextNode . data of textnode

# 3rd Task: Get an array of all the "name" elements
namesArray = xmldoc.getElementsByTagName("name")
