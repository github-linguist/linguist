// stdlib/semver.k — semantic version parse & compare in PURE KRYPTON. No C.
//
// Handles MAJOR.MINOR.PATCH with an optional "-prerelease" and "+build". Build
// metadata is ignored in comparison (per semver.org); a prerelease sorts BEFORE
// its release ("1.0.0-rc" < "1.0.0").
//
//   semverCmp("1.2.0", "1.10.0")  -> -1   (numeric, not lexical)
//   semverGte("2.1.0", "2.0.9")   -> "1"
//   semverMajor("1.2.3")          -> 1
//   semverSatisfiesCaret("^1.2.0", "1.5.0") -> "1"

module semver

// strip a leading "v" and any "+build" metadata.
func _clean(v) {
    let s = v
    if startsWith(s, "v") { s = substring(s, 1, len(s)) }
    let plus = indexOf(s, "+")
    if plus >= 0 { s = substring(s, 0, plus) }
    emit s
}

func _core(v) {
    let s = _clean(v)
    let dash = indexOf(s, "-")
    if dash >= 0 { emit substring(s, 0, dash) }
    emit s
}

func _pre(v) {
    let s = _clean(v)
    let dash = indexOf(s, "-")
    if dash >= 0 { emit substring(s, dash + 1, len(s)) }
    emit ""
}

// idx-th dot-separated field of the core, as an int. Avoids list builtins
// (splitBy lists are unreliable on the native backend) — pure indexOf scan.
func _part(v, idx) {
    let c = _core(v)
    let start = 0
    let field = 0
    while field < idx {
        let rest = substring(c, start, len(c))
        let dot = indexOf(rest, ".")
        if dot < 0 { emit 0 }
        start = start + dot + 1
        field += 1
    }
    let rest = substring(c, start, len(c))
    let dot = indexOf(rest, ".")
    if dot >= 0 { emit toInt(substring(rest, 0, dot)) }
    emit toInt(rest)
}

func semverMajor(v) { emit _part(v, 0) }
func semverMinor(v) { emit _part(v, 1) }
func semverPatch(v) { emit _part(v, 2) }

func _cmpInt(a, b) {
    if a < b { emit 0 - 1 }
    if a > b { emit 1 }
    emit 0
}

// semverCmp(a, b) -> -1 if a<b, 0 if equal, 1 if a>b.
func semverCmp(a, b) {
    let c0 = _cmpInt(semverMajor(a), semverMajor(b))
    if c0 != 0 { emit c0 }
    let c1 = _cmpInt(semverMinor(a), semverMinor(b))
    if c1 != 0 { emit c1 }
    let c2 = _cmpInt(semverPatch(a), semverPatch(b))
    if c2 != 0 { emit c2 }
    // cores equal -> compare prerelease: a release outranks a prerelease.
    let pa = _pre(a)
    let pb = _pre(b)
    if pa == "" {
        if pb == "" { emit 0 }
        emit 1                  // a is release, b is prerelease -> a > b
    }
    if pb == "" { emit 0 - 1 }  // a prerelease, b release -> a < b
    // both prerelease: simple lexical compare via charCode
    let i = 0
    while i < len(pa) {
        if i >= len(pb) { emit 1 }
        let ca = toInt(charCode(pa[i]))
        let cb = toInt(charCode(pb[i]))
        if ca < cb { emit 0 - 1 }
        if ca > cb { emit 1 }
        i += 1
    }
    if len(pb) > len(pa) { emit 0 - 1 }
    emit 0
}

func semverEq(a, b)  { if semverCmp(a, b) == 0 { emit "1" } emit "0" }
func semverLt(a, b)  { if semverCmp(a, b) < 0  { emit "1" } emit "0" }
func semverGt(a, b)  { if semverCmp(a, b) > 0  { emit "1" } emit "0" }
func semverLte(a, b) { if semverCmp(a, b) <= 0 { emit "1" } emit "0" }
func semverGte(a, b) { if semverCmp(a, b) >= 0 { emit "1" } emit "0" }

// semverSatisfiesCaret("^1.2.3", v): same major, and v >= 1.2.3. ("1" / "0")
func semverSatisfiesCaret(range, v) {
    let base = range
    if startsWith(base, "^") { base = substring(base, 1, len(base)) }
    if semverMajor(v) != semverMajor(base) { emit "0" }
    emit semverGte(v, base)
}

// semverSatisfiesTilde("~1.2.3", v): same major+minor, and v >= 1.2.3.
func semverSatisfiesTilde(range, v) {
    let base = range
    if startsWith(base, "~") { base = substring(base, 1, len(base)) }
    if semverMajor(v) != semverMajor(base) { emit "0" }
    if semverMinor(v) != semverMinor(base) { emit "0" }
    emit semverGte(v, base)
}
