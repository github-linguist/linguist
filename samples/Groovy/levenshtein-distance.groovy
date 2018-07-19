def distance(String str1, String str2) {
    def dist = new int[str1.size() + 1][str2.size() + 1]
    (0..str1.size()).each { dist[it][0] = it }
    (0..str2.size()).each { dist[0][it] = it }

    (1..str1.size()).each { i ->
        (1..str2.size()).each { j ->
            dist[i][j] = [dist[i - 1][j] + 1, dist[i][j - 1] + 1, dist[i - 1][j - 1] + ((str1[i - 1] == str2[j - 1]) ? 0 : 1)].min()
        }
    }
    return dist[str1.size()][str2.size()]
}

[ ['kitten', 'sitting']: 3,
  ['rosettacode', 'raisethysword']: 8,
  ['edocattesor', 'drowsyhtesiar']: 8 ].each { key, dist ->
    println "Checking distance(${key[0]}, ${key[1]}) == $dist"
    assert distance(key[0], key[1]) == dist
}
