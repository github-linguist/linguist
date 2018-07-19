$date = [DateTime]::Parse("March 7 2009 7:30pm -5" )
write-host $date
write-host $date.AddHours(12)
write-host [TimeZoneInfo]::ConvertTimeBySystemTimeZoneId($date.AddHours(12),"Vladivostok Standard Time")
