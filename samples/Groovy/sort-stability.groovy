def cityList = ['UK  London', 'US  New York', 'US  Birmingham', 'UK  Birmingham',].asImmutable()
[
    'Sort by city': { city -> city[4..-1] },
    'Sort by country': { city -> city[0..3] },
].each{ String label, Closure orderBy ->
    println "\n\nBefore ${label}"
    cityList.each { println it }
    println "\nAfter ${label}"
    cityList.sort(false, orderBy).each{ println it }
}
