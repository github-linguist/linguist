library(RJSONIO)
langUrl <- "http://rosettacode.org/mw/api.php?action=query&format=json&cmtitle=Category:Solutions_by_Programming_Language&list=categorymembers&cmlimit=500"

languages <- fromJSON(langUrl)$query$categorymembers
languages <- sapply(languages, function(x) sub("Category:", "", x$title))

# fails if there are more than 500 users per language
user <- function (lang) {
  userBaseUrl <- "http://rosettacode.org/mw/api.php?action=query&format=json&list=categorymembers&cmlimit=500&cmtitle=Category:"
  userUrl <- paste(userBaseUrl, URLencode(paste(lang, " User", sep="")),sep="")
  length(fromJSON(userUrl)$query$categorymembers)
}

users <- sapply(languages, user)
head(sort(users, decreasing=TRUE),15)
