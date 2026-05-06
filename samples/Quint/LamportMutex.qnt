module LamportMutex {
  // This is a typed version of LamportMutex, which can be found at:
  // https://github.com/tlaplus/Examples/blob/master/specifications/lamport_mutex/LamportMutex.tla

  /***************************************************************************/
  /* TLA+ specification of Lamport's distributed mutual-exclusion algorithm  */
  /* that appeared as an example in                                          */
  /* L. Lamport:  Time, Clocks and the Ordering of Events in a Distributed   */
  /* System. CACM 21(7):558-565, 1978.                                       */
  /***************************************************************************/

  // The parameter N represents the number of processes.
  const N: int
  // The parameter maxClock is used only for model checking in order to
  // keep the state space finite.
  const maxClock: int

  assume NType = N.in(Nat)
  assume maxClockType = maxClock.in(Nat)

  val Proc = 1.to(N)
  val Clock = Nat.exclude(Set(0))

  /* For model checking, add ClockConstraint as a state constraint to ensure
  a finite state space and override the definition of Clock by
  1 .. maxClock+1 so that TLC can evaluate the definition of Message. */

  // local clock of each process
  var clock: int -> int
  // requests received from processes (clock transmitted with request)
  var req: int -> (int -> int)
  // acknowledgements received from processes
  var ack: int -> Set[int]
  // set of processes in critical section
  var crit: Set[int]
  // messages sent but not yet received
  var network: int -> (int -> List[{ mtype: str, clock: int }])

  // Messages used in the algorithm.
  def ReqMessage(c) = { mtype: "req", clock: c }
  val AckMessage = { mtype: "ack", clock: 0 }
  val RelMessage = { mtype: "rel", clock: 0 }

  val Message = Set(AckMessage, RelMessage).union(Clock.map(c => ReqMessage(c)))

  action Init = all {
      clock' = Proc.mapBy(p => 0),
      req' = Proc.mapBy(p => Proc.mapBy(q => 0)),
      ack' = Proc.mapBy(p => Set()),
      network' = Proc.mapBy(p => Proc.mapBy(q => List())),
      crit' = Set(),
  }

  /* beats(p,q) is true if process p believes that its request has higher
     priority than q's request. This is true if either p has not received a
     request from q or p's request has a smaller clock value than q's.
     If there is a tie, the numerical process ID decides. */
  def beats(p, q) = or {
    req.get(p).get(q) == 0,
    req.get(p).get(p) < req.get(p).get(q),
    req.get(p).get(p) == req.get(p).get(q),
    p < q,
  }

  // Broadcast a message: send it to all processes except the sender. 
  def Broadcast(s, m) = Proc.mapBy(r =>
    if (r == s)
      network.get(s).get(r)
    else 
      network.get(s).get(r).append(m)
  )

  // Process p requests access to critical section.
  action Request(p) = all {
    req.get(p).get(p) == 0,
    req' = req.setBy(p, r => r.setBy(p, c => clock.get(p))),
    network' = network.setBy(p, n => Broadcast(p, ReqMessage(clock.get(p)))),
    ack' = ack.set(p,Set(p)),
    clock' = clock,
    crit' = crit,
  }

  // Process p receives a request from q and acknowledges it.
  action ReceiveRequest(p,q) = all {
    network.get(p).get(q) != List(),
    val m = network.get(q).get(p).head()
    val c = m.clock
    all {
      m.mtype == "req",
      req' = req.setBy(p, a => a.setBy(q, b => c)),
      clock' = clock.setBy(p, a => if (c > a) c + 1 else a + 1),
      network' = network.setBy(q, a => a.setBy(p, b => b.tail()))
                        .setBy(p, a => a.setBy(q, b => b.append(AckMessage))),
    },
    ack' = ack,
    crit' = crit,
  }

  // Process p receives an acknowledgement from q. 
  action ReceiveAck(p, q) = all {
    network.get(p).get(q) != List(),
    val m = network.get(p).get(q).head() all {
      m.mtype == "ack",
      network' = network.setBy(p, a => a.setBy(q, b => b.tail())),
      ack' = ack.setBy(p, a => a.union(Set(q))),
    },
    clock' = clock,
    req' = req,
    crit' = crit,
  }

  // Process p enters the critical section.
  action Enter(p) = all {
    ack.get(p) == Proc,
    Proc.forall(q => beats(p, q)),
    crit' = crit.union(Set(p)),
    clock' = clock,
    req' = req,
    ack' = ack,
    network' = network,
  }

  // Process p exits the critical section and notifies other processes.
  action Exit(p) = all {
    p.in(crit),
    crit' = crit.exclude(Set(p)),
    network' = network.set(p, Broadcast(p, RelMessage)),
    req' = req.setBy(p, r => r.set(p, 0)),
    ack' = ack.set(p, Set()),
    clock' = clock,
  }

  // Process p receives a release notification from q.
  action ReceiveRelease(p,q) = all {
    network.get(p).get(q) != List(),
    val m = network.get(p).get(q).head() all {
      m.mtype == "rel",
      req' = req.setBy(p, r => r.set(q, 0)),
      network' = network.setBy(p, a => a.setBy(q, b => b.tail())),
    },
    clock' = clock,
    ack' = ack,
    crit' = crit,
  }

  // Next-state relation.
  action Next = {
    nondet p = oneOf(Proc)
    any {
      Request(p),
      Enter(p),
      Exit(p),
      nondet q = oneOf(Proc)
      any {
        ReceiveRequest(p,q),
        ReceiveAck(p,q),
        ReceiveRelease(p,q),
      }
    }
  }  

  /* A state constraint that is useful for validating the specification
     using finite-state model checking. */
  val ClockConstraint = Proc.forall(p => (clock.get(p) <= maxClock))

  /* No channel ever contains more than three messages. In fact, no channel
     ever contains more than one message of the same type, as proved below.*/
  val BoundedNetwork = Proc.forall(p =>
    Proc.forall(q => 
      network.get(p).get(q).length() <= 3
    )
  )

  // The main safety property of mutual exclusion.
  val Mutex = crit.forall(p => crit.forall(q => p == q))
}

module LamportMutex_3_10 {
  import LamportMutex(
    N = 3,
    maxClock = 10
  ).*
}
