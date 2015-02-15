import datetime, calendar

DISCORDIAN_SEASONS = ["Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"]

def ddate(year, month, day):
    today = datetime.date(year, month, day)
    is_leap_year = calendar.isleap(year)
    if is_leap_year and month == 2 and day == 29:
        return "St. Tib's Day, YOLD " + (year + 1166)

    day_of_year = today.timetuple().tm_yday - 1

    if is_leap_year and day_of_year >= 60:
        day_of_year -= 1 # Compensate for St. Tib's Day

    season, dday = divmod(day_of_year, 73)
    return "%s %d, YOLD %d" % (DISCORDIAN_SEASONS[season], dday + 1, year + 1166)
