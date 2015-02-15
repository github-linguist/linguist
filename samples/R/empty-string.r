s <- ''

if (s == '') cat('Empty\n')
#or
if (nchar(s) == 0) cat('Empty\n')

if (s != '') cat('Not empty\n')
#or
if (nchar(s) > 0) cat('Not empty\n')
