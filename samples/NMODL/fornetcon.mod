: Each NetCon maintains a count of external events.
: And time of last external event
: On each internal event all connecting NetCon get the internal event count.
: And time of last external event

NEURON {
  POINT_PROCESS ForNetConTest
  RANGE tbegin
}

UNITS {
}

PARAMETER {
  tbegin = 0 (ms)
}

INITIAL {
  net_send(tbegin, 1)
}

NET_RECEIVE(w, npre, tpre (ms), npost, tpost (ms)) {
  INITIAL {
    npre=0  tpre=-1  npost=0  tpost=-1
  }

  if (flag == 0) { : external (pre) event
    npre = npre + 1
    tpre = t
  }

  if (flag == 1) { : internal (post) event
    FOR_NETCONS(w, fnpre, ftpre (ms), fnpost, ftpost (ms)) {
      fnpost = fnpost + 1
      ftpost = t
    }
    net_send(3, 1) : in 3 ms another 1 event
    net_event(t)
  }
}
