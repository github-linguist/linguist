## Require the XML package you can download from http://www.omegahat.org/RSXML/
library("XML")
doc <- xmlInternalTreeParse("test3.xml")
#  1st Task: Retrieve the first "item" element
(firstItemElement <- getNodeSet(doc, "//item")[[1]])
# 2nd task: Perform an action on each "price" element (print it out)
prices <- sapply(getNodeSet(doc, "//price"), xmlValue)
for(i in 1:length(prices)) print(prices[i])
# 3rd Task: Get an array of all the "name" elements
(namesArray <- sapply(getNodeSet(doc, "//name"), xmlValue))
