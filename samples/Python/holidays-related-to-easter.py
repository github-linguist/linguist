from dateutil.easter import *
import datetime, calendar

class Holiday(object):
    def __init__(self, date, offset=0):
        self.holiday = date + datetime.timedelta(days=offset)

    def __str__(self):
        dayofweek = calendar.day_name[self.holiday.weekday()][0:3]
        month = calendar.month_name[self.holiday.month][0:3]
        return '{0} {1:2d} {2}'.format(dayofweek, self.holiday.day, month)

def get_holiday_values(year):
    holidays = {'year': year}
    easterDate = easter(year)
    holidays['easter'] = Holiday(easterDate)
    holidays['ascension'] = Holiday(easterDate, 39)
    holidays['pentecost'] = Holiday(easterDate, 49)
    holidays['trinity'] = Holiday(easterDate, 56)
    holidays['corpus'] = Holiday(easterDate, 60)
    return holidays

def print_holidays(holidays):
    print '{year:4d} Easter: {easter}, Ascension: {ascension}, Pentecost: {pentecost}, Trinity: {trinity}, Corpus: {corpus}'.format(**holidays)

if __name__ == "__main__":
    print "Christian holidays, related to Easter, for each centennial from 400 to 2100 CE:"
    for year in range(400, 2200, 100):
        print_holidays(get_holiday_values(year))

    print ''
    print "Christian holidays, related to Easter, for years from 2010 to 2020 CE:"
    for year in range(2010, 2021):
        print_holidays(get_holiday_values(year))
