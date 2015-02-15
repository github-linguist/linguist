'Enjoy Rosetta Code'.tokenize().collect { w ->
    Thread.start {
        Thread.sleep(1000 * Math.random() as int)
        println w
    }
}.each { it.join() }
