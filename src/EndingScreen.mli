(** This module handles the ending screen and the the printing at the
    end of a game. *)

open State
(** Contains all functionality for printing the end screen from the
    [.txt] file in [end_screen_txt] *)

val end_screen_txt : string
(** [end_screen_txt] is the name of the end screen file to be printed.*)

val stats_screen_txt : string
(** [stats_screen] is the name of the stats screen file to be printed.*)

val make_end_screen : string -> t -> bool -> unit
(**[make_end_screen] is the printing mechanism for printing the end
   screen to the terminal. Input [f] is the name of the text file to be
   printed. [a'] is a placeholder for now about how the game should be
   played/passing in the function that actually plays the game. [b]
   specifies whether the user has successfully made it through all
   possible days *)

val prompt_player_finish : unit -> unit
(** [prompt_player_start] prompts the player to press "play", "help", or
    "quit" to begin the game. This function is recursive to prompt
    multiple times until the correct input is given. [play_function] is
    the function to begin the game.*)
