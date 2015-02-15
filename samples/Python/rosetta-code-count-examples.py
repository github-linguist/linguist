import urllib, xml.dom.minidom

x = urllib.urlopen("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml")

tasks = []
for i in xml.dom.minidom.parseString(x.read()).getElementsByTagName("cm"):
    t = i.getAttribute('title').replace(" ", "_")
    y = urllib.urlopen("http://www.rosettacode.org/w/index.php?title=%s&action=raw" % t.encode('utf-8'))
    tasks.append( y.read().lower().count("{{header|") )
    print t.replace("_", " ") + ": %d examples." % tasks[-1]

print "\nTotal: %d examples." % sum(tasks)
