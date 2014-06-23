# from
# http://wiki.erights.org/wiki/Walnut/Advanced_Topics/Build_your_Own_Guards
def makeVOCPair(brandName :String) :near {

    var myTempContents := def none {}

    def brand {
        to __printOn(out :TextWriter) :void {
            out.print(brandName)
        }
    }

    def ProveAuth {
        to __printOn(out :TextWriter) :void {
            out.print(`<$brandName prover>`)
        }
        to getBrand() :near { return brand }
        to coerce(specimen, optEjector) :near {
            def sealedBox {
                to getBrand() :near { return brand }
                to offerContent() :void {
                    myTempContents := specimen
                }
            }
            return sealedBox
        }
    }
    def CheckAuth {
        to __printOn(out :TextWriter) :void {
            out.print(`<$brandName checker template>`)
        }
        to getBrand() :near { return brand }
        match [`get`, authList :any[]] {
            def checker {
                to __printOn(out :TextWriter) :void {
                    out.print(`<$brandName checker>`)
                }
                to getBrand() :near { return brand }
                to coerce(specimenBox, optEjector) :any {
                    myTempContents := null
                    if (specimenBox.__respondsTo("offerContent", 0)) {
                      # XXX Using __respondsTo/2 here is a kludge
                        specimenBox.offerContent()
                    } else {
                        myTempContents := specimenBox
                    }
                    for auth in authList {
                        if (auth == myTempContents) {
                            return auth
                        }
                    }
                    myTempContents := none
                    throw.eject(optEjector,
                                `Unmatched $brandName authorization`)
                }
            }
        }
        match [`__respondsTo`, [`get`, _]] {
            true
        }
        match [`__respondsTo`, [_, _]] {
            false
        }
        match [`__getAllegedType`, []] {
            null.__getAllegedType()
        }
    }
    return [ProveAuth, CheckAuth]
}
