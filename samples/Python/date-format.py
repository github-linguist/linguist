import datetime
today = datetime.date.today()
# This one is built in:
print today.isoformat()
# Or use a format string for full flexibility:
print today.strftime('%Y-%m-%d')
