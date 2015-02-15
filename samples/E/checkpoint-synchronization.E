/** A flagSet solves this problem: There are N things, each in a true or false
  * state, and we want to know whether they are all true (or all false), and be
  * able to bulk-change all of them, and all this without allowing double-
  * counting -- setting a flag twice is idempotent.
  */
def makeFlagSet() {
  # Each flag object is either in the true set or the false set.
  def trues := [].asSet().diverge()
  def falses := [].asSet().diverge()
  return def flagSet {
    /** Add a flag to the set. */
    to join() {
      def flag {
        /** Get the value of this flag. */
        to get() :boolean {

        }
        /** Set the value of this flag. */
        to put(v :boolean) {
          def [del,add] := if (v) { [falses,trues] } else { [trues,falses] }
          if (del.contains(flag)) {
            del.remove(flag)
            add.addElement(flag)
          }
        }
        /** Remove this flag from the set. */
        to leave() :void {
          trues.remove(flag)
          falses.remove(flag)
        }
      }
      falses.addElement(flag)
      return flag
    }
    /** Are all the flags true (none false)? */
    to allTrue() { return falses.size().isZero() }
    /** Are all the flags false (none true)? */
    to allFalse() { return trues.size().isZero() }
    /** Set all the flags to the same value. */
    to setAll(v :boolean) {
      def [del,add] := if (v) { [falses,trues] } else { [trues,falses] }
      add.addAll(del)
      del.removeAll(del)
    }
  }
}

def makeCheckpoint() {
  def [var continueSignal, var continueRes] := Ref.promise()
  def readies := makeFlagSet()

  /** Check whether all tasks have reached the checkpoint, and if so send the
    * signal and go to the next round. */
  def check() {
    if (readies.allTrue()) {
      readies.setAll(false)

      continueRes.resolve(null)    # send the continue signal

      def [p, r] := Ref.promise()  # prepare a new continue signal
      continueSignal := p
      continueRes := r
    }
  }

  return def checkpoint {
    to join() {
      def &flag := readies.join()
      return def membership {
        to leave() {
          (&flag).leave()
          check <- ()
        }
        to deliver() {
          flag := true
          check <- ()
          return continueSignal
        }
      }
    }
  }
}

def makeWorker(piece, checkpoint) {
  def stops := timer.now() + 3000 + entropy.nextInt(2000)
  var count := 0
  def checkpointMember := checkpoint <- join()
  def stopped
  def run() {
    # Pretend to do something lengthy; up to 1000 ms.
    timer.whenPast(timer.now() + entropy.nextInt(1000), fn {
      if (timer.now() >= stops) {
        checkpointMember <- leave()
        bind stopped := true
      } else {
        count += 1
        println(`Delivering $piece#$count`)
        when (checkpointMember <- deliver()) -> {
          println(`Delivered $piece#$count`)
          run()
        }
      }
    })
  }
  run()
  return stopped
}

def checkpoint := makeCheckpoint()
var waits := []
for piece in 1..5 {
  waits with= makeWorker(piece, checkpoint)
}
interp.waitAtTop(promiseAllFulfilled(waits))
