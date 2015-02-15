def html = new URL('http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000').getText([
        connectTimeout:500,
        readTimeout:15000,
        requestProperties: [ 'User-Agent': 'Firefox/2.0.0.4']])
def count = [:]
(html =~ '<li><a[^>]+>([^<]+)</a>[^(]*[(](\\d+) member[s]*[)]</li>').each { match, language, members ->
    count[language] = (members as int)
}
count.sort { v1, v2 -> v2.value <=> v1.value }.eachWithIndex { value, index -> println "${index + 1} $value" }
