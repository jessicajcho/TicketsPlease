open Game

(** [play_game f] starts the game. *)
let play_game () : unit =
  let play t : unit = () in
  play (GameDay.next_days (State.init_state ""))

(** [main ()] prompts for main screen items, then starts the game upon
    play. *)
let main () =
  TitleScreen.make_title_screen TitleScreen.title_screen_txt;
  TitleScreen.prompt_player_start ();
  Random.self_init ();
  play_game ();
  let rec replay () =
    TitleScreen.prompt_player_start ();
    play_game ();
    replay ()
  in
  replay ()
(* let rec replay () = Endingscreen.prompt_player_finish (); replay ()
   in replay () *)

(* Execute the game engine. *)
let () = main ()
