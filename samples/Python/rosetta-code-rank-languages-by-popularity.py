import urllib, re

key1 = lambda x: int(x[1])

get1 = urllib.urlopen("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Languages&cmlimit=500&format=json").read()
get2 = urllib.urlopen("http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000").read()

langs = re.findall("\"title\":\"Category:(.+?)\"",get1)
qtdmbr = re.findall("title=\"Category:(.+?)\">.+?</a> \((\d+) members\)",get2)

result = [(x,int(y)) for x,y in qtdmbr if x in langs]

for n, i in enumerate(sorted(result,key=key1,reverse=True)):
    print "%3d. %3d - %s" % (n+1, i[1], i[0])
