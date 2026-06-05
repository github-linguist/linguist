// json.k — Simple JSON builder for Krypton
// Produces JSON strings from Krypton values

module json

func jsonStr(s) {
    emit "\"" + replace(replace(s, "\\", "\\\\"), "\"", "\\\"") + "\""
}

func jsonNum(n) {
    emit n + ""
}

func jsonBool(b) {
    if b == "1" || b == "true" { emit "true" }
    emit "false"
}

func jsonNull() {
    emit "null"
}

func jsonArray(lst) {
    if len(lst) == 0 { emit "[]" }
    let sb = sbNew()
    sb = sbAppend(sb, "[")
    let cnt = length(lst)
    let i = 0
    for item in lst {
        if i > 0 { sb = sbAppend(sb, ",") }
        sb = sbAppend(sb, jsonStr(item))
        i += 1
    }
    sb = sbAppend(sb, "]")
    emit sbToString(sb)
}

func jsonObject(map) {
    let sb = sbNew()
    sb = sbAppend(sb, "{")
    let ks = keys(map)
    let cnt = length(ks)
    let i = 0
    for k in ks {
        if i > 0 { sb = sbAppend(sb, ",") }
        sb = sbAppend(sb, jsonStr(k) + ":" + jsonStr(mapGet(map, k)))
        i += 1
    }
    sb = sbAppend(sb, "}")
    emit sbToString(sb)
}
