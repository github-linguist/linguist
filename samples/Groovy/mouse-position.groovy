1.upto(5) {
    Thread.sleep(1000)
    def p = java.awt.MouseInfo.pointerInfo.location
    println "${it}: x=${p.@x} y=${p.@y}"
}
