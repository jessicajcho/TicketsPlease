(** This module represents special events. *)

val money_change : State.t -> float
(**[money_change t] determines the float value to change the money of a
   state t by during a special event. *)
