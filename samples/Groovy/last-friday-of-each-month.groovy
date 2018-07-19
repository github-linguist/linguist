def ymd = { it.format('yyyy-MM-dd') }
def lastFridays = lastWeekDays.curry(Day.Fri)
lastFridays(args[0] as int).each { println (ymd(it)) }
