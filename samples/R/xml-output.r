library(XML)
char2xml <- function(names, remarks){
	tt <- xmlHashTree()
	head <- addNode(xmlNode("CharacterRemarks"), character(), tt)
	node <- list()
	for (i in 1:length(names)){
		node[[i]] <- addNode(xmlNode("Character", attrs=c(name=names[i])), head, tt)
		addNode(xmlTextNode(remarks[i]), node[[i]], tt)
	}
	return(tt)
}
output <- char2xml( names=c("April","Tam O'Shanter","Emily"),
remarks=c("Bubbly: I'm > Tam and <= Emily", 'Burns: "When chapman billies leave the street ..."', "Short & shrift") )
