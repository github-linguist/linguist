YahooSearch <- function(query, page=1, .opts=list(), ignoreMarkUpErrors=TRUE)
{
   if(!require(RCurl) || !require(XML))
   {
      stop("Could not load required packages")
   }

   # Replace " " with "%20", etc
   query <- curlEscape(query)

   # Retrieve page
   b <- 10*(page-1)+1
   theurl <- paste("http://uk.search.yahoo.com/search?p=",
      query, "&b=", b, sep="")
   webpage <- getURL(theurl, .opts=.opts)

   # Save search for nextpage function
   .Search <- list(query=query, page=page, .opts=.opts,
      ignoreMarkUpErrors=ignoreMarkUpErrors)
   assign(".Search", .Search, envir=globalenv())

   # Parse HTML; retrieve results block
   webpage <- readLines(tc <- textConnection(webpage)); close(tc)
   if(ignoreMarkUpErrors)
   {
      pagetree <- htmlTreeParse(webpage, error=function(...){})
   } else
   {
      pagetree <- htmlTreeParse(webpage)
   }


   findbyattr <- function(x, id, type="id")
   {
      ids <- sapply(x, function(x) x$attributes[type])
      x[ids==id]
   }

   body <- pagetree$children$html$children$body
   bd <- findbyattr(body$children$div$children, "bd")
   left <- findbyattr(bd$div$children$div$children, "left")
   web <- findbyattr(left$div$children$div$children, "web")
   resol <- web$div$children$ol

   #Get url, title, content from results
   gettextfromnode <- function(x)
   {
      un <- unlist(x$children)
      paste(un[grep("value", names(un))], collapse=" ")
   }

   n <- length(resol)
   results <- list()
   length(results) <- n
   for(i in 1:n)
   {
      mainlink <- resol[[i]]$children$div$children[1]$div$children$h3$children$a
      url <- mainlink$attributes["href"]
      title <- gettextfromnode(mainlink)

      contenttext <- findbyattr(resol[[i]]$children$div$children[2], "abstr", type="class")
      if(length(contenttext)==0)
      {
          contenttext <- findbyattr(resol[[i]]$children$div$children[2]$div$children$div$children,
            "sm-abs", type="class")
      }

      content <- gettextfromnode(contenttext$div)
      results[[i]] <- list(url=url, title=title, content=content)
   }
   names(results) <- as.character(seq(b, b+n-1))
   results
}

nextpage <- function()
{
   if(exists(".Search", envir=globalenv()))
   {
      .Search <- get(".Search", envir=globalenv())
      .Search$page  <- .Search$page + 1L
      do.call(YahooSearch, .Search)
   } else
   {
      message("No search has been performed yet")
   }
}

#Usage
YahooSearch("rosetta code")
nextpage()
