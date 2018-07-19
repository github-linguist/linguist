year = commandArgs(T)
d = as.Date(paste0(year, "-01-01"))
fridays = d + seq(by = 7,
    (5 - as.POSIXlt(d)$wday) %% 7,
    364 + (months(d + 30 + 29) == "February"))
message(paste(collapse = "\n", fridays[tapply(
    seq_along(fridays), as.POSIXlt(fridays)$mon, max)]))
