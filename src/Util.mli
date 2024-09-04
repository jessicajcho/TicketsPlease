(** This module handles various multi-purpose functions used in the rest
    of the implementation of the game. *)

val explode : string -> char list
(**[explode s] explodes string [s] into an array of its characters*)

val implode : char list -> string
(**[implode l] joins a character list [l] into the string it represents.*)

val replace_string_char : string -> int -> char -> string
(**[replace_string_char s i chr] replaces the character in position [i]
   in string [s] to [chr]*)

val rand_elem : 'a list -> 'a
(**[rand_elem l] returns a random element in [l].*)

val retrieve_string : string -> string

(**[retrieve_string str] is the string literal in [printing.json] that
   corresponds to tag [str]. Raises [Not_found] if [str] is an invalid
   keyword*)
