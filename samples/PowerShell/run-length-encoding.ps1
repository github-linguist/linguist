function Compress-RLE ($s) {
    $re = [regex] '(.)\1*'
    $ret = ""
    foreach ($m in $re.Matches($s)) {
        $ret += $m.Length
        $ret += $m.Value[0]
    }
    return $ret
}

function Expand-RLE ($s) {
    $re = [regex] '(\d+)(.)'
    $ret = ""
    foreach ($m in $re.Matches($s)) {
        $ret += [string] $m.Groups[2] * [int] [string] $m.Groups[1]
    }
    return $ret
}
