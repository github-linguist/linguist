def makeIntegrator() {
    var value := 0.0
    var input := fn { 0.0 }

    var input1 := input()
    var t1 := timer.now()

    def update() {
        def t2 := timer.now()
        def input2 :float64 := input()
        def dt := (t2 - t1) / 1000

        value += (input1 + input2) * dt / 2

        t1 := t2
        input1 := input2
    }

    var task() {
        update <- ()
        task <- ()
    }
    task()

    def integrator {
        to input(new) :void  { input := new }
        to output() :float64 { return value }
        to shutdown()        { task := fn {} }
    }
    return integrator
}

def test() {
    def result

    def pi := (-1.0).acos()
    def freq := pi / 1000

    def base := timer.now()
    def i := makeIntegrator()

    i.input(fn { (freq * timer.now()).sin() })
    timer.whenPast(base + 2000, fn {
        i.input(fn {0})
    })
    timer.whenPast(base + 2500, fn {
        bind result := i.output()
        i.shutdown()
    })
    return result
}
