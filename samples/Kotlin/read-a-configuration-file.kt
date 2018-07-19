data class Configuration(val map: Map<String, Any?>) {
    val fullName: String by Delegates.mapVal(map)
    val favoriteFruit: String by Delegates.mapVal(map)
    val needsPeeling: Boolean by Delegates.mapVal(map)
    val otherFamily: List<String> by Delegates.mapVal(map)
}

fun main(args: Array<String>) {
    val configurationPath = Paths.get(args[0])!!

    val configurables = Files.readAllLines(configurationPath, StandardCharsets.UTF_8)
                             .map { it.trim() }
                             .filterNot { it.isEmpty() }
                             .filterNot(::commentedOut)
                             .map(::toKeyValuePairs)

    val configurationMap: MutableMap<String, Any?> = hashMapOf("needsPeeling" to false)
    for (configurable in configurables) {
        val (key, value) = configurable
        when (key) {
            "FULLNAME"       -> configurationMap.put("fullName", value)
            "FAVOURITEFRUIT" -> configurationMap.put("favoriteFruit", value)
            "NEEDSPEELING"   -> configurationMap.put("needsPeeling", true)
            "OTHERFAMILY"    -> configurationMap.put("otherFamily", value.split(" , ").map { it.trim() })
            else             -> println("Encountered unexpected key ${key}=${value}")
        }
    }

    val configuration = Configuration(configurationMap)
}

private fun commentedOut(line: String): Boolean = (line.indexOf("#") == 0 || line.indexOf(";") == 0)

private fun toKeyValuePairs(line: String): Pair<String, String> {
    return line.split(" ", 2).let {
        Pair(it[0], if (it.size == 1) "" else it[1])
    }
}
