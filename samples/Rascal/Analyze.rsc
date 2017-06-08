module Analyze

import Syntax;

set[Id] unreachable(Machine m) {
  r = { <q1,q2> | (State)`state <Id q1> <Trans* ts>` <- m.states, 
				  (Trans)`<Id _>: <Id q2>` <- ts }+;
  qs = [ q.name | /State q := m ];
  return { q | q <- qs, q notin r[qs[0]] };
}
