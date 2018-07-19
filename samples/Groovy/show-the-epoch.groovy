def date = new Date(0)
def format = new java.text.SimpleDateFormat('yyyy-MM-dd\'T\'HH:mm:ss.SSSZ')
format.timeZone = TimeZone.getTimeZone('UTC')
println (format.format(date))
