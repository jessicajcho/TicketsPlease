(** This module represents the current dynamic game state. *)

(*Representation of dynamic game state. Anything in this state should be
  accessible to all parts of gameplay. Only include things that we may
  need to persist beyond a single state.

  Example: Number of correct accepts/rejects should persist throughout
  the game *)

type t
(** The abstract type of values representing the game state. *)

val init_state : string -> t
(** Initializes the game state.*)

val make_decision : Command.command -> t -> bool -> t
(** [make_decision cmd st is_valid] updates game state [st] based on
    accept/reject [cmd] and if the person being processed [is_valid] *)

val advance_day : t -> t
(** [advance_day st] advances day [st] to the next day in the game *)

val change_days_left : t -> int -> t
(** [change_days_left st days] changes the value of days left in state
    [st] to [days] *)

val decr_days_left : t -> t
(** [decr_days_left st] decrements the value of days left in state [st]
    by 1 *)

val get_days_left : t -> int
(** [get_days_left st] is the value of the days left in [st] *)

val get_index_day : t -> int
(** [get_index_day st] returns the current index day of the game (0
    indexed on the first day) in [st] *)

val change_money : t -> int -> t
(** [change_money st money] changes the amount of money user will have
    in [st] to add [money] *)

val buy_item : t -> int -> t
(** [buy_item st item] subtracts the cost of the [item] from net_money
    in [st]. *)

val update_money : t -> t
(** [update_money st] updates amount of money user has each day based on
    how they have played *)

val update_location : t -> string -> t
(** [update_location st loc] updates location [loc] of game stat [st] *)

val get_current_day : t -> Date.date
(** [get_curennt_day st] is the current game date *)

val reset_guesses : t -> t
(** [reset_guesses st] resets the guess statistics in [st]*)

val incr_correct_guess : t -> t
(** [incr_correct_guesses st] increments the number of correct guesses
    in [st] by 1 *)

val money_total : t -> int
(** [money_total st ] gets the current total money in [st] *)

val money_change : t -> int
(** [money_change st] is the total amount of money gained/lost (if
    negative) *)

val get_inventory : t -> string list
(** [get_inventory st] is the inventory of player in current state [st] *)

val add_to_inventory : t -> string -> t
(** [add_to_inventory st item] is the state [st] with [item] added to
    the inventory. *)

val remove_from_inventory : t -> string -> t
(** [remove_from_inventory st item] is the state [st] with the [item]
    removed from the inventory. *)

val get_guess_stats : t -> int * int
(** [get_guess_stats st] is (# of correct guesses, # of total guesses)
    in state [st]*)

val get_total_stats : t -> int list
(** [get_total_stats st] is all relevant statistics for the end of the
    game. *)

val get_special_event : t -> bool
(** [get_special_event st] is a [true] if a special event is triggered
    and false otherwise.*)

val update_state_reason : t -> string -> t
(**[update_state_reason st reason] updates the incorrect_reason to
   [reason] to print for the current person being processed.*)

val print_inventory : t -> string
(**[print_inventory st] returns the string that contains all the user's
   current inventory items*)

val get_location : t -> string
(**[get_location st] returns the current location*)