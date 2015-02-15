library(XML)
find.unimplemented.tasks <- function(lang="R"){
	PT <- xmlInternalTreeParse( paste("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml",sep="") )
	PT.nodes <- getNodeSet(PT,"//cm")
	PT.titles = as.character( sapply(PT.nodes, xmlGetAttr, "title") )
	language <- xmlInternalTreeParse( paste("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:",
	lang, "&cmlimit=500&format=xml",sep="") )
	lang.nodes <- getNodeSet(language,"//cm")
	lang.titles = as.character( sapply(lang.nodes, xmlGetAttr, "title") )
	unimplemented <- setdiff(PT.titles, lang.titles)
	unimplemented
}
# Usage
find.unimplemented.tasks(lang="Python")
langs <- c("R","python","perl")
sapply(langs, find.unimplemented.tasks) # fetching data for multiple languages
