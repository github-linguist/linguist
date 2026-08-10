/**
 * Clock Synchronization Algorithm - Version 6 (from TLA+ ClockSync6)
 *
 * This specification is a Quint translation of the TLA+ model ClockSync6.tla
 * from the tla-apalache-workshop [1].
 *
 * Check with:
 *   quint verify clockSync6.qnt --invariant skew_inv --main clock_sync4
 *
 * [1]: https://github.com/informalsystems/tla-apalache-workshop/blob/main/examples/clock-sync/ClockSync6.tla
 */
module clock_sync {
  import basicSpells.* from "../../../spells/basicSpells"

  /* CONSTANTS */

  /// The set of processes
  const processes: Set[str]
  /// Minimum message delay
  const t_min: int
  /// Maximum message delay
  const t_max: int

  assume _ = (t_min >= 0 and t_max > t_min)

  /* TYPES */

  type ControlState = Init | Sent | Sync

  type Message = { src: str, timestamp: int }

  type State = {
    process_id: str,
    hardware_clock: int,
    clock_adjustment: int,
    diffs: str -> int,
    received_messages: Set[Message],
    control_state: ControlState,
  }

  /* State Variables */

  /// The reference clock, inaccessible to the processes
  var time: int
  /// Messages sent by the processes
  var messages: Set[Message]
  /// The local state of each process
  var states: str -> State

  /* DEFINITIONS */

  /// Sum up the clock differences as observed by a process p
  pure def diff_sum(s: State): int = {
    s.diffs.values().fold(0, (total, d) => total + d)
  }

  /// If the process has received a message from all processes (but p),
  /// then adjust the clock.
  pure def adjust_clock(s: State): State = {
    if (s.received_messages.map(m => m.src) == processes.setRemove(s.process_id))
      // Adjust the clock
      {
        ...s,
        clock_adjustment: diff_sum(s) / processes.size(),
        control_state: Sync,
      }
    else
      s
  }

  /* ACTIONS */

  /// Send the value of the hardware clock
  action send_msg(s: State): bool = all {
    s.control_state == Init,
    messages' = messages.setAdd({ src: s.process_id, timestamp: s.hardware_clock }),
    states' = states.set(s.process_id, { ...s, control_state: Sent }),
    time' = time,
  }

  // Receive a message sent by another process.
  // Adjust the clock if the message has been received from all other processes.
  action receive_msg(s: State): bool = all {
    s.control_state == Sent,

    val potential_messages = messages.filter(m =>
      // Not already received
      not(m.in(s.received_messages)) and
      // Not from itself
      m.src != s.process_id and
      // The message cannot be received earlier than after t_min
      // (enforced by the system, the process can't control this)
      states.get(m.src).hardware_clock >= m.timestamp + t_min
    )

    all {
      potential_messages != Set(),
      // This choice implies a precondition that the filtered set is non-empty
      nondet m = oneOf(potential_messages)

      // Accumulate the difference and adjust the clock if possible
      val delta = m.timestamp - s.hardware_clock + (t_min + t_max) / 2
      val new_state = {
        ...s,
        received_messages: s.received_messages.union(Set(m)),
        diffs: s.diffs.set(m.src, delta),
      }
      states' = states.set(s.process_id, adjust_clock(new_state)),
      time' = time,
      messages' = messages,
    }
  }

  // Let the time flow
  action advance_clocks(delta_param: int): bool = all {
    delta_param > 0,
    // Clocks can be advanced only if there is no pending message
    messages.forall(m =>
      states.get(m.src).hardware_clock + delta_param > t_max
        implies states.values().forall(s =>
          m.in(s.received_messages)
        )
    ),
    time' = time + delta_param,
    states' = states.transformValues(s => {
      ...s,
      hardware_clock: s.hardware_clock + delta_param
    }),
    messages' = messages,
  }

  action init = all {
    time' = 0,
    messages' = Set(),
    states' = processes.mapBy(p => {
      process_id: p,
      hardware_clock: 0,
      clock_adjustment: 0,
      diffs: processes.mapBy(_ => 0),
      control_state: Init,
      received_messages: Set(),
    })
  }

  action step = any {
    // Option 1: Advance clocks
    nondet delta = oneOf(Int)
    advance_clocks(delta),

    // Option 2: A process performs an action
    nondet s = oneOf(states.values())
    any {
      s.send_msg(),
      s.receive_msg(),
    }
  }

  /* Properties */

  /// The adjusted clock of process i_proc
  def adjusted_clock(i_proc: str): int = {
    val s = states.get(i_proc)
    s.hardware_clock + s.clock_adjustment
  }

  // Theorem 6.15 from AW04:
  // Algorithm achieves u * (1 - 1/n)-synchronization for n processors.
  val skew_inv: bool = {
    val all_sync = states.values().forall(s => s.control_state == Sync)

    val bounded_skew = {
      val N = processes.size()
      // extend the bound by N to account for rounding errors
      val bound = (t_max - t_min) * (N - 1) + N * N
      processes.forall(p =>
        processes.forall(q => {
          val df = p.adjusted_clock() - q.adjusted_clock()
          -bound <= df * N and df * N <= bound
        })
      )
    }
    all_sync implies bounded_skew
  }
}

module clock_sync4 {
  import clock_sync(
    processes = Set("p1", "p2", "p3", "p4"),
    t_min = 17,
    t_max = 91
  ).*
}
