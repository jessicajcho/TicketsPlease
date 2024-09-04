open TerminalPrint

let title_screen_txt = "data/title_screen.txt"

let rec prompt_player_start () =
  try
    match
      Command.parse (prompt_user (Util.retrieve_string "start_game"))
    with
    | Play -> ()
    | Quit ->
        print_endline "\nThanks for playing! Goodbye.\n";
        exit 0
    | Help ->
        print_endline (Util.retrieve_string "title_help");
        prompt_player_start ()
    | _ -> prompt_player_start ()
  with
  | Command.Malformed -> prompt_player_start ()
  | Command.Empty -> prompt_player_start ()

(**Pass in the file to make the screen*)
let make_title_screen f =
  print_file f;
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Tickets, Please: Alert Level Red!\n\n"
