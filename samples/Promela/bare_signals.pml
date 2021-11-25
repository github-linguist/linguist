inline bare_signals_0_transition(id)
{
  int transition_id;
  transition_id = id;
  do
  ::(transition_id == -1)->
    break;
  ::(transition_id == 0)->
    bare_signals_0_state = bare_signals_0_state_1;
    transition_id = -1;
  ::(transition_id == 1)->
    bare_signals_0_state = bare_signals_0_state_2;
    transition_id = -1;
  ::(transition_id == 2)->
    bare_signals_0_state = bare_signals_0_state_1;
    transition_id = -1;
  od;
}
inline bare_signals_0_init()
{
  bare_signals_0_transition(0);
}
inline bare_signals_0_PI_0_signal_1()
{
  if
  ::(bare_signals_0_state == bare_signals_0_state_1)->
    bare_signals_0_transition(1);
  ::else->
    break;
  fi;
}
inline bare_signals_0_PI_0_signal_2()
{
  if
  ::(bare_signals_0_state == bare_signals_0_state_2)->
    bare_signals_0_transition(2);
  ::else->
    break;
  fi;
}
