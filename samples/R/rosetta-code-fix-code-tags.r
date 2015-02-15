fixtags <- function(page)
{
   langs <- c("c", "c-sharp", "r")   # a complete list is required, obviously
   langs <- paste(langs, collapse="|")
   page <- gsub(paste("<(", langs, ")>", sep=""), "<lang \\1>", page)
   page <- gsub(paste("</(", langs, ")>", sep=""), "</#####lang>", page)
   page <- gsub(paste("<code(", langs, ")>", sep=""), "<lang \\1>", page)
   page <- gsub(paste("</code>", sep=""), "</#####lang>", page)
   page
}

page <- "lorem ipsum <c>some c code</c>dolor sit amet,<c-sharp>some c-sharp code</c-sharp>
consectetur adipisicing elit,<code r>some r code</code>sed do eiusmod tempor incididunt"
fixtags(page)
