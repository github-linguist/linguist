isLeapYear <- function(year) {
    ifelse(year%%100==0, year%%400==0, year%%4==0)
}

for (y in c(1900, 1994, 1996, 1997, 2000)) {
    print(paste(y, " is ", ifelse(isLeapYear(y), "", "not "), "a leap year.", sep=""))
}
