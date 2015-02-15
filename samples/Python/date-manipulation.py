import datetime

def mt():
	datime1="March 7 2009 7:30pm EST"
	formatting = "%B %d %Y %I:%M%p "
	datime2 = datime1[:-3]  # format can't handle "EST" for some reason
	tdelta = datetime.timedelta(hours=12)		# twelve hours..
	s3 = datetime.datetime.strptime(datime2, formatting)
	datime2 = s3+tdelta
	print datime2.strftime("%B %d %Y %I:%M%p %Z") + datime1[-3:]

mt()
