def move(out, n, fromPeg, toPeg, viaPeg) {
    if (n.aboveZero()) {
        move(out, n.previous(), fromPeg, viaPeg, toPeg)
        out.println(`Move disk $n from $fromPeg to $toPeg.`)
        move(out, n.previous(), viaPeg, toPeg, fromPeg)
    }
}

move(stdout, 4, def left {}, def right {}, def middle {})
