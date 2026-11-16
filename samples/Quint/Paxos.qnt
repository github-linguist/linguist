
module Paxos {
  /***************************************************************************
   * This is a specification of the Paxos algorithm without explicit leaders *
   * or learners.  It refines the spec in Voting                             *
   ***************************************************************************/
  /***************************************************************************
   * The constant parameters and the set Ballots are the same as in Voting.  *
   ***************************************************************************/

  const Value: Set[int]

  const Acceptor: Set[str]

  const Quorum: Set[Set[str]]

  assume _ = and {
      Quorum.forall(Q => Q.subseteq(Acceptor)),
      tuples(Quorum, Quorum).forall( ((Q1, Q2)) => (Q1.intersect(Q2)) != Set())
  }

  val Ballot = Nat

  val None = -1
    /*************************************************************************
     * An unspecified value that is not a ballot number.                     *
     *************************************************************************/

  //-----------------------------------------------------------------------------

  var maxBal:   str -> int
  var maxVBal:  str -> int // <<maxVBal[a], maxVal[a]>> is the vote with the largest
  var maxVal:   str -> int  // ballot number cast by a; it equals <<-1, None>> if
                            // a has not cast any vote.

  // Particular message types we need to refer to later:
  type Msg1bT = { acceptor: str, bal: int, mbal: int, mval: int }
  type Msg2aT = { bal: int, value: int }

  // A union type for records representing different types of messages.
  type MSG =
      | Msg1a({ bal: int })
      | Msg1b(Msg1bT)
      | Msg2a(Msg2aT)
      | Msg2b({ acc: str, bal: int, value: int })

  var msgs: Set[MSG] // The set of all messages that have been sent.

  // Access the balance of a message, irrespictive of the message variant
  def balance(m: MSG): int = match m {
    | Msg1a(mm) => mm.bal
    | Msg1b(mm) => mm.bal
    | Msg2a(mm) => mm.bal
    | Msg2b(mm) => mm.bal
  }

  // Filter a set of messager into just 1b messages
  def filterMsg1b(ms: Set[MSG]): Set[Msg1bT] =
    ms.fold(Set(), (acc, m) => match m {
      | Msg1b(m1b) => acc.union(Set(m1b))
      | _          => acc
    })

  // Filter a set of messager into just 2a messages
  def filterMsg2a(ms: Set[MSG]): Set[Msg2aT] =
    ms.fold(Set(), (acc, m) => match m {
      | Msg2a(m2a) => acc.union(Set(m2a))
      | _          => acc
    })

  /***************************************************************************)
  (* NOTE:                                                                   *)
  (* The algorithm is easier to understand in terms of the set msgs of all   *)
  (* messages that have ever been sent.  A more accurate model would use     *)
  (* one or more variables to represent the messages actually in transit,    *)
  (* and it would include actions representing message loss and duplication  *)
  (* as well as message receipt.                                             *)
  (*                                                                         *)
  (* In the current spec, there is no need to model message loss because we  *)
  (* are mainly concerned with the algorithm's safety property.  The safety  *)
  (* part of the spec says only what messages may be received and does not   *)
  (* assert that any message actually is received.  Thus, there is no        *)
  (* difference between a lost message and one that is never received.  The  *)
  (* liveness property of the spec that we check makes it clear what         *)
  (* messages must be received (and hence either not lost or successfully    *)
  (* retransmitted if lost) to guarantee progress.                           *)
  (***************************************************************************/

  val vars = (maxBal, maxVBal, maxVal, msgs)
    /*************************************************************************
     * It is convenient to define some identifier to be the tuple of all     *
     * variables.  I like to use the identifier `vars'.                      *
     *************************************************************************/

  /***************************************************************************)
  (* The type invariant and initial predicate.                               *)
  (***************************************************************************/
  val TypeOK = and {
      maxBal.in(Acceptor.setOfMaps(Ballot.union(Set(-1)))),
      maxVBal.in(Acceptor.setOfMaps(Ballot.union(Set(-1)))),
      maxVal.in(Acceptor.setOfMaps(Value.union(Set(None)))),
  }

  action Init = all {
      maxBal'  = Acceptor.mapBy(_ => -1),
      maxVBal' = Acceptor.mapBy(_ => -1),
      maxVal'  = Acceptor.mapBy(_ => None),
      msgs'    = Set(),
  }

  /***************************************************************************)
  (* The actions.  We begin with the subaction (an action that will be used  *)
  (* to define the actions that make up the next-state action.               *)
  (***************************************************************************/
  action Send(m: MSG): bool = {
      msgs' = msgs.union(Set(m))
  }

  /***************************************************************************)
  (* In an implementation, there will be a leader process that orchestrates  *)
  (* a ballot.  The ballot b leader performs actions Phase1a(b) and          *)
  (* Phase2a(b).  The Phase1a(b) action sends a phase 1a message (a message  *)
  (* m with m.type = "1a") that begins ballot b.                             *)
  (***************************************************************************/
  action Phase1a(b) = all {
      Send(Msg1a({ bal: b })),
      maxVBal' = maxVBal,
      maxVal'  = maxVal,
  }

  /***************************************************************************)
  (* Upon receipt of a ballot b phase 1a message, acceptor a can perform a   *)
  (* Phase1b(a) action only if b > maxBal[a].  The action sets maxBal[a] to  *)
  (* b and sends a phase 1b message to the leader containing the values of   *)
  (* maxVBal[a] and maxVal[a].                                               *)
  (***************************************************************************/
  action Phase1b(a) = {
      nondet m = msgs.filter(m2 => match m2 { Msg1a(_) => true | _ => false }).oneOf()
      all {
          m.balance() > maxBal.get(a),
          Send(Msg1b({ acceptor: a, bal: m.balance(), mbal: maxVBal.get(a), mval: maxVal.get(a) })),
          maxBal' = maxBal.set(a, m.balance()),
          maxVBal' = maxVBal,
          maxVal'  = maxVal,
      }
  }

  /***************************************************************************)
  (* The Phase2a(b, v) action can be performed by the ballot b leader if two *)
  (* conditions are satisfied: (i) it has not already performed a phase 2a   *)
  (* action for ballot b and (ii) it has received ballot b phase 1b messages *)
  (* from some quorum Q from which it can deduce that the value v is safe at *)
  (* ballot b.  These enabling conditions are the first two conjuncts in the *)
  (* definition of Phase2a(b, v).  This second conjunct, expressing          *)
  (* condition (ii), is the heart of the algorithm.  To understand it,       *)
  (* observe that the existence of a phase 1b message m in msgs implies that *)
  (* m.mbal is the highest ballot number less than m.bal in which acceptor   *)
  (* m.acc has or ever will cast a vote, and that m.mval is the value it     *)
  (* voted for in that ballot if m.mbal # -1.  It is not hard to deduce from *)
  (* this that the second conjunct implies that there exists a quorum Q such *)
  (* that ShowsSafeAt(Q, b, v) (where ShowsSafeAt is defined in module       *)
  (* Voting).                                                                *)
  (*                                                                         *)
  (* The action sends a phase 2a message that tells any acceptor a that it   *)
  (* can vote for v in ballot b, unless it has already set maxBal[a]         *)
  (* greater than b (thereby promising not to vote in ballot b).             *)
  (***************************************************************************/
  action Phase2a(b, v) = all {
    not(msgs.exists(m => match m { Msg2a(m2a) => m2a.bal == b | _ => false })),
    Quorum.exists(Q =>
      val Q1b = msgs.filterMsg1b()
                    .filter(m => (m.acceptor.in(Q)) and (m.bal == b))
      val Q1bv = Q1b.filter(m => m.mbal >= 0)
      and {
          Q.forall(a => Q1b.exists(m => m.acceptor == a)),
          or {
              Q1bv == Set(),
              Q1bv.exists(m => or {
                  m.mval == v,
                  Q1bv.forall(mm => m.mbal >= mm.mbal)
              })
          }
      }
    ),
    Send(Msg2a({ bal: b, value: v })),
    maxVBal' = maxVBal,
    maxVal'  = maxVal,
  }

  /***************************************************************************)
  (* The Phase2b(a) action is performed by acceptor a upon receipt of a      *)
  (* phase 2a message.  Acceptor a can perform this action only if the       *)
  (* message is for a ballot number greater than or equal to maxBal[a].  In  *)
  (* that case, the acceptor votes as directed by the phase 2a message,      *)
  (* setting maxVBal[a] and maxVal[a] to record that vote and sending a      *)
  (* phase 2b message announcing its vote.  It also sets maxBal[a] to the    *)
  (* message's.  ballot number                                               *)
  (***************************************************************************/
  action Phase2b(a) = {
    nondet m = msgs.filterMsg2a().oneOf()
    all {
      m.bal >= maxBal.get(a),
      Send(Msg2b({ acc: a, bal: m.bal, value: m.value })),
      maxBal'  = maxBal.set(a, m.bal),
      maxVBal' = maxVBal.set(a, m.bal),
      maxVal'  = maxVal.set(a, m.value),
    }
  }

  /***************************************************************************)
  (* In an implementation, there will be learner processes that learn from   *)
  (* the phase 2b messages if a value has been chosen.  The learners are     *)
  (* omitted from this abstract specification of the algorithm.              *)
  (***************************************************************************/

  /***************************************************************************)
  (* Below are defined the next-state action and the complete spec.          *)
  (***************************************************************************/
  action Next = any {
    all {
      nondet b = oneOf(Ballot)
      any {
        Phase1a(b),
        nondet v = oneOf(Value)
        Phase2a(b, v)
      },
      maxBal' = maxBal,
    },
    nondet a = oneOf(Acceptor)
    any {
      Phase1b(a),
      Phase2b(a),
    }
  }

  //----------------------------------------------------------------------------
  /***************************************************************************)
  (* We now define the refinement mapping under which this algorithm         *)
  (* implements the specification in module Voting.                          *)
  (***************************************************************************/

  /***************************************************************************)
  (* As we observed, votes are registered by sending phase 2b messages.  So  *)
  (* the array `votes' describing the votes cast by the acceptors is defined *)
  (* as follows.                                                             *)
  (***************************************************************************/
  val votes =
    Acceptor.mapBy(a =>
      msgs.fold(Set(), (acc, mm) =>
        match mm {
        | Msg2b(m) => if (m.acc == a) acc.union(Set((m.bal, m.value))) else acc
        | _ => acc }))

  /***************************************************************************)
  (* We now instantiate module Voting, substituting the constants Value,     *)
  (* Acceptor, and Quorum declared in this module for the corresponding      *)
  (* constants of that module Voting, and substituting the variable maxBal   *)
  (* and the defined state function `votes' for the correspondingly-named    *)
  (* variables of module Voting.                                             *)
  (***************************************************************************/


  import Voting(Value = Value, Acceptor = Acceptor, Quorum = Quorum) as V from "./Voting"

  // Like theorems? Then you should definitely go straight for TLA+
  // THEOREM Spec => V!Spec
  //-----------------------------------------------------------------------------
  /***************************************************************************)
  (* Here is a first attempt at an inductive invariant used to prove this    *)
  (* theorem.                                                                *)
  (***************************************************************************/
  val Inv = and {
     TypeOK,
     Acceptor.forall(a =>
         if (maxVBal.get(a) == -1)
         maxVal.get(a) == None
         else (maxVBal.get(a), maxVal.get(a)).in(votes.get(a))
     ),
     msgs.filterMsg1b()
         .forall(m =>
            (maxBal.get(m.acceptor) >= m.bal
                and ((m.mbal >= 0) implies (m.mbal, m.mval).in(votes.get(m.acceptor))))),
     val M2as = msgs.filterMsg2a()
     M2as.forall(m => and {
        Quorum.exists(Q => V::ShowsSafeAt(Q, m.bal, m.value)),
        M2as.forall(mm =>
            (mm.bal == m.bal) implies (mm.value == m.value)
        ),
     }),
     V::Inv,
  }
}

module Paxos_val2_accept3_quorum2 {
  import Paxos(
    Value = Set(0, 1),
    Acceptor = Set("a1", "a2", "a3"),
    Quorum = Set(Set("a1", "a2"))
  ).*
}
