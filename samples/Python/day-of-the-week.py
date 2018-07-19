import datetime

def yuletide():
   sunday = 6
   days = (day.strftime('%d %b %Y') for day in (datetime.date(year, 12, 25) for year in range(2008,2122)) if day.weekday() == sunday)
   print '\n'.join(days)

yuletide()
