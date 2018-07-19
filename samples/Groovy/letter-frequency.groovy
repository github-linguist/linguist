def frequency = { it.inject([:]) { map, value -> map[value] = (map[value] ?: 0) + 1; map } }

frequency(new File('frequency.groovy').text).each { key, value ->
    println "'$key': $value"
}
