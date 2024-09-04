(** Contains all functionality for printing the initial title screen
    from the [.txt] file in [title_screen_txt] *)

val title_screen_txt : string
(** [title_screen_txt] is the name of the title screen file to be
    printed.*)

val make_title_screen : string -> unit
(**[make_title_screen] is the printing mechanism for printing the title
   screen to the terminal. Input [f] is the name of the text file to be
   printed. [a'] is a placeholder for now about how the game should be
   played/passing in the function that actually plays the game *)

val prompt_player_start : unit -> unit
(** [prompt_player_start] prompts the player to press "play", "help", or
    "quit" to begin the game. This function is recursive to prompt
    multiple times until the correct input is given. [play_function] is
    the function to begin the game. *)