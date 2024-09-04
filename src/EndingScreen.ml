open TerminalPrint
open State

let end_screen_txt = "data/end_screen.txt"
let stats_screen_txt = "data/stats_screen.txt"

let manage_quit () =
  print_endline "\nThanks for playing! Goodbye.\n";
  exit 0

let rec prompt_player_finish () =
  try
    match
      Command.parse (prompt_user (Util.retrieve_string "start_game"))
    with
    | Play -> ()
    | Quit -> manage_quit ()
    | Help -> manage_help ()
    | _ -> prompt_player_finish ()
  with
  | Command.Malformed -> prompt_player_finish ()
  | Command.Empty -> prompt_player_finish ()

and manage_help () =
  print_endline "\nNot implemented\n";
  prompt_player_finish ()

let overall_game_stats (st : t) =
  let intro = "Here are your stats for today." in
  print_file stats_screen_txt;
  print_delay_char intro 0.07 false;
  erase_delay_char (String.length intro) 0.07;
  let stats = get_total_stats st in
  print_delay_char
    ("You encountered " ^ string_of_int (List.nth stats 0) ^ " people.")
    0.07 true;
  print_delay_char
    ("You got " ^ string_of_int (List.nth stats 3) ^ " of them correct.")
    0.07 true

(**Pass in the file to make the screen. This function is called after
   the game has finished and it produces a game-end screen as well as a
   stats page. *)
let make_end_screen f (st : t) (days_finished : bool) =
  if days_finished then
    print_delay_char
      "Congratulations! You have successfully completed all of the days"
      0.07 true
  else print_endline "";
  Unix.sleepf 1.;
  overall_game_stats st;
  print_file f;
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nThank you for playing Tickets, Please: Alert Level Red!\n\n"
