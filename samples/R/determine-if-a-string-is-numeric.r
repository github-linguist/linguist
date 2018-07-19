> strings <- c("152", "-3.1415926", "Foo123")
> suppressWarnings(!is.na(as.numeric(strings)))
[1]  TRUE  TRUE FALSE
