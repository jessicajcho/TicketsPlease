(** This module handles the game day and the passage of days in the
    game. *)

open State

val num_people_per_day : int
(** This is the number of people the player will interact with per day. *)

val num_days : int
(** This is the number of people the player will interact with per day. *)

val next_person : t -> t
(** Introduces another person to the player. PLayer chooses to accept or
    reject the person. Returns the game state after the person is
    handled by the player. *)

val day_end : t -> t
(** Takes the game state after the main gameplay is finished and handles
    the end-of-day sequence *)

val game_end : t -> unit
(** Handles the sequence of events that occur after the game ends and
    the days left reach 0 *)

val next_day : t -> t
(** Takes in the current game state and returns the game state after one
    day. *)

val next_days : t -> unit
(** Takes in the current game state and returns the game state after
    days pass. *)
