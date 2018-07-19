swap <- function(name1, name2, envir = parent.env(environment()))
{
    temp <- get(name1, pos = envir)
    assign(name1, get(name2, pos = envir), pos = envir)
    assign(name2, temp, pos = envir)
}
