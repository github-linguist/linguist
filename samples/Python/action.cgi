#!/usr/bin/python

from model import Feed
import session
import datetime
import sys

argv = session.argv()

feed = Feed.get(guid=argv[1])
action = argv[2]

if action == 'done':
    when = feed.notify_interval * feed.notify_unit
elif action == 'snooze':
    if len(argv) > 3:
        when = int(argv[3])
    else:
        when = 3600
else:
    print '''Status: 400 Bad request
Content-type: text/html

Unknown action %s''' % action
    sys.exit(1)

feed.notify_next = datetime.datetime.utcnow() + datetime.timedelta(seconds=when)
feed.save()

response = '''Content-type: text/html

<html><head><title>Alarm reset</title>
<link rel="stylesheet" href="{base_url}/style.css">
</head>
<body>

<div class="container">
<h1>Alarm reset</h1>
<div>
<p id="reset">Alarm "<span class="name">{name}</span>" has been reset. You won't be notified for another <span class="duration">{duration}</span>.</p>

<p>Actions:</p>
<ul>
<li><a href="{edit_url}?feed={guid}">Edit this reminder</a></li>
<li><a href="{edit_url}">Create another reminder</a></li>
<li><a href="{base_url}">Visit the Reminder Me site</a></li>
</ul>
</div>
</div>

<p class="back"><a href=".">Reminder Me</a></p>

</body></html>'''

when_left = when
duration_list = []
for (label,period) in [('month',86400*365/12),
                       ('week',86400*7),
                       ('day',86400),
                       ('hour',3600),
                       ('minute',60),
                       ('second',1)]:
    if when == period:
        duration_list = [label]
        break
    
    val = when_left/period
    if val:
        duration_list.append("%d %s%s" % (
            val,
            label,
            val > 1 and 's' or ''))
        when_left -= val*period

basedir=session.request_script_dir()

print response.format(guid=feed.guid,
                      name=feed.name,
                      edit_url="%s/edit.cgi" % basedir,
                      base_url=basedir,
                      duration=', '.join(duration_list))
                      
