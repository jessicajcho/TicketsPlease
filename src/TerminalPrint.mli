(** This module handles the various printing functionality in the game. *)

open Util

val read_from_file : string -> string list
(**[read_from_file] takes in any file [f] and outputs a list of the
   file's contents. Each list item is a line of the file, and the 0th
   entry of the list is the first line of [f]*)

val prompt_user : string -> string
(**[prompt_user] prints a user prompt and outputs the user string
   response from the terminal. [str] is the message that should be used
   to prompt the user. No newline (\n) characters are needed in the
   string, as these will be added to the user prompt in this function.*)

val print_file : string -> unit
(**[print_file f] prints the entire contents of file [f] to the terminal*)

val print_list : string list -> unit
(**[print_list l] prints the entire contents of [l], with each element
   on a new line*)

val erase_delay_char : int -> float -> unit
(**[print_delay_char l s] erase [l] characters individually with a time
   delay of [t] between each character. *)

val print_delay_char : string -> float -> bool -> unit
(**[print_delay_char t s b] prints [s] with a time delay of [t] between
   each character. It moves cursor to the next line if [b] is true. *)

val print_delay_string : string list -> float -> unit
(** [print_delay_string l t] prints [l] string by string with a time
    delay of [t] between each string. It prints each string on its own
    line. *)
