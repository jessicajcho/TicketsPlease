let money_change (t : State.t) =
  if State.get_special_event t then
    Random.float 1. *. float_of_int (State.money_total t)
  else 0.0
