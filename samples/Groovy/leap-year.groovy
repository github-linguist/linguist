(1900..2012).findAll {new GregorianCalendar().isLeapYear(it)}.each {println it}
