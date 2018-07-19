def makeDelegator {
    /** construct without an explicit delegate */
    to run() {
        return makeDelegator(null)
    }

    /** construct with a delegate */
    to run(delegateO) { # suffix because "delegate" is a reserved keyword
        def delegator {
            to operation() {
                return if (delegateO.__respondsTo("thing", 0)) {
                           delegateO.thing()
                       } else {
                           "default implementation"
                       }
            }
        }
        return delegator
    }
}

? def delegator := makeDelegator()
> delegator.operation()
# value: "default implementation"

? def delegator := makeDelegator(def doesNotImplement {})
> delegator.operation()
# value: "default implementation"

? def delegator := makeDelegator(def doesImplement {
>     to thing() { return "delegate implementation" }
> })
> delegator.operation()
# value: "default implementation"
