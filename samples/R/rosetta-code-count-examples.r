library(XML)
library(RCurl)
doc <- xmlInternalTreeParse("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml")
nodes <- getNodeSet(doc,"//cm")
titles = as.character( sapply(nodes, xmlGetAttr, "title") )
headers <- list()
counts <- list()
for (i in 1:length(titles)){
	headers[[i]] <- getURL( paste("http://rosettacode.org/mw/index.php?title=", gsub(" ", "_", titles[i]), "&action=raw", sep="") )
	counts[[i]] <- strsplit(headers[[i]],split=" ")[[1]]
	counts[[i]] <- grep("\\{\\{header", counts[[i]])
	cat(titles[i], ":", length(counts[[i]]), "examples\n")
}
cat("Total: ", length(unlist(counts)), "examples\n")
