import urllib
import re

def fix(x):
    p = re.compile(r'<[^<]*?>')
    return p.sub('', x).replace('&amp;', '&')

class YahooSearch:
    def __init__(self, query, page=1):
        self.query = query
        self.page = page
        self.url = "http://search.yahoo.com/search?p=%s&b=%s" %(self.query, ((self.page - 1) * 10 + 1))
        self.content = urllib.urlopen(self.url).read()

    def getresults(self):
        self.results = []

        for i in re.findall('<a class="yschttl spt" href=".+?">(.+?)</a></h3></div>(.+?)</div>.*?<span class=url>(.+?)</span>', self.content):

            title = fix(i[0])
            content = fix(i[1])
            url = fix(i[2])

            self.results.append(YahooResult(title, content, url))

        return self.results

    def getnextpage(self):
        return YahooSearch(self.query, self.page+1)

    search_results = property(fget=getresults)
    nextpage = property(fget=getnextpage)

class YahooResult:
    def __init__(self,title,content,url):
        self.title = title
        self.content = content
        self.url = url

# Usage:

x = YahooSearch("test")

for result in x.search_results:
    print result.title
