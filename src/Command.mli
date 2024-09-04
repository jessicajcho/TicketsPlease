(** This module involves parsing of player's inputted commands. *)

(*We have the option of changing how we parse user commands. We could
  parse them as lists of words, full strings, etc*)

type object_phrase = string list
(** [object_phrase] represents a list of strings passed after a user
    enters input. *)

type command =
  | Play
  | Accept of object_phrase
  | Reject of object_phrase
  | Buy of object_phrase
  | Continue
  | Help
  | Quit
  | Rules
  | Inventory
  | Money

(** [command] represents the different types of command the user can
    provide to the game.*)

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is encountered. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command]*)
