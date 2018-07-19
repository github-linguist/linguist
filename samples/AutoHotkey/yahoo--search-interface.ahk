test:
yahooSearch("test", 1)
yahooSearch("test", 2)
return

yahooSearch(query, page)
{
  global
  start := ((page - 1) * 10) + 1
  filedelete, search.txt
  urldownloadtofile, % "http://search.yahoo.com/search?p=" . query
  . "&b=" . start, search.txt
  fileread, content, search.txt
  reg = <a class="yschttl spt" href=".+?" >(.+?)</a></h3></div><div class="abstr">(.+?)</div><span class=url>(.+?)</span>

  index := found := 1
  while (found := regexmatch(content, reg, self, found + 1))
  {
    msgbox % title%A_Index% := fix(self1)
    content%A_Index% := fix(self2)
    url%A_Index% := fix(self3)
  }
}

fix(url)
{
if pos := instr(url, "</a></h3></div>")
StringLeft, url, url, pos - 1
url := regexreplace(url, "<.*?>")
return url
}
