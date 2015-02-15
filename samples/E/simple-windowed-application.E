when (currentVat.morphInto("awt")) -> {
    var clicks := 0
    def w := <swing:makeJFrame>("Rosetta Code 'Simple Windowed Application'")
    w.setContentPane(JPanel`
        ${def l := <swing:makeJLabel>("There have been no clicks yet.")} $\
            ${def b := <swing:makeJButton>("Click Me")}
    `)
    b.addActionListener(def _ {
        to actionPerformed(_) {
            clicks += 1
            l.setText(`Number of clicks: $clicks`)
        }
    })
    w.pack()
    w.show()
}
