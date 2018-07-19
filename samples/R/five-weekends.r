ms = as.Date(sapply(c(1, 3, 5, 7, 8, 10, 12),
    function(month) paste(1900:2100, month, 1, sep = "-")))
ms = format(sort(ms[weekdays(ms) == "Friday"]), "%b %Y")
message("There are ", length(ms), " months with five weekends.")
message("The first five: ", paste(ms[1:5], collapse = ", "))
message("The last five: ", paste(tail(ms, 5), collapse = ", "))
