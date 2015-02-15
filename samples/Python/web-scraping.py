import urllib
page = urllib.urlopen('http://tycho.usno.navy.mil/cgi-bin/timer.pl')
for line in page:
    if ' UTC' in line:
        print line.strip()[4:]
        break
page.close()
