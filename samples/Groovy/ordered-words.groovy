def isOrdered = { word -> def letters = word as List; letters == ([] + letters).sort() }
assert isOrdered('abbey')
assert !isOrdered('cat')

def dictUrl = new URL('http://www.puzzlers.org/pub/wordlists/unixdict.txt')
def orderedWords = dictUrl.readLines().findAll { isOrdered(it) }
def owMax = orderedWords*.size().max()

orderedWords.findAll { it.size() == owMax }.each { println it }
