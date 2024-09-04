open State
open Documents
open TerminalPrint
open Command
open FinalPerson
open EndingScreen
open Yojson.Basic.Util
open Util
open ParseRules

let num_people_per_day = 3
let num_days = ParseRules.num_days
let time_limit = 10.

let from_json json =
  let places = member "collection" json |> to_list in
  let rec get_attrib lst =
    match lst with
    | [] -> []
    | h :: t ->
        let loc = to_string @@ member "location" h in
        let descript = to_string @@ member "location description" h in
        (loc, descript) :: get_attrib t
  in
  get_attrib places

let rec try_accept_reject (st : State.t) (is_valid : bool) =
  try
    make_decision
      (parse (prompt_user (Util.retrieve_string "user_prompt")))
      st is_valid
  with
  | Command.Empty ->
      print_endline "\nSorry, this isn't a real command. Try again?";

      try_accept_reject st is_valid
  | Command.Malformed ->
      print_endline "\nSorry, this isn't a real command. Try again?";
      try_accept_reject st is_valid

let next_person (st : t) =
  let n_person = make_final_person st in
  let is_valid =
    FinalPerson.get_person n_person |> Person.check_valid_docs
  in

  let reason = get_reason_string n_person in
  let new_st = State.update_state_reason st reason in
  print_documents (get_documents n_person);
  try_accept_reject new_st is_valid

let print_intro_stats st correct_guess total_guess =
  ANSITerminal.print_string
    [ ANSITerminal.Background Green; ANSITerminal.Bold ]
    ("\nDAY " ^ string_of_int (State.get_index_day st + 1) ^ " STATS");
  print_endline
    "\nThe day is now over. Here are your statistics for today: ";
  Unix.sleep 1;
  print_endline
    ("\nOut of the "
    ^ string_of_int total_guess
    ^ " people you met, you made the right decision for "
    ^ string_of_int correct_guess
    ^ " people.")

let print_money_diff st =
  if money_change st < 0 then
    print_endline
      ("You have lost $" ^ string_of_int (0 - money_change st))
  else
    print_endline ("You have gained $" ^ string_of_int (money_change st))

let print_money_sustain st =
  print_endline
    ("You now have: $" ^ string_of_int (money_total st) ^ "\n");
  reset_guesses (Inventory.shop st)

let print_special_event change str =
  print_endline
    (Util.retrieve_string str ^ string_of_float change ^ " dollars!")

let handle_special_event st new_st =
  let change = RandomEvents.money_change st in
  if change = 0. then print_money_sustain new_st
  else
    let sp_new_st =
      update_money
        (change_money
           (update_money (State.advance_day st))
           (int_of_float change))
    in
    if change > 0. then print_special_event change "spec_1"
    else print_special_event change "spec_2";
    print_endline
      ("You now have: $" ^ string_of_int (money_total sp_new_st) ^ "\n");
    reset_guesses (Inventory.shop sp_new_st)

let day_end (st : t) =
  let guess_stats = get_guess_stats st in
  let correct_guess = fst guess_stats in
  let total_guess = snd guess_stats in
  print_intro_stats st correct_guess total_guess;
  print_money_diff st;
  let new_st = update_money (State.advance_day st) in
  if money_total new_st < 0 then (
    print_endline (Util.retrieve_string "kicked_out_no_money");
    change_days_left new_st 0)
  else handle_special_event st new_st

let game_end (st : t) =
  EndingScreen.make_end_screen EndingScreen.end_screen_txt st
    (get_days_left st = 0)

let next_day (st : t) =
  let start_time = Unix.gettimeofday () in
  let rec next_day_helper (st : t) (ppl : int) =
    if Unix.gettimeofday () -. start_time < time_limit then
      next_day_helper (next_person st) 0
    else
      let _ =
        print_endline "\nYour time is up.";
        print_endline
          ("\n" ^ ParseRules.get_day_end_story (State.get_index_day st))
      in
      day_end st
  in
  next_day_helper st 0

let print_beginning_day st =
  ANSITerminal.print_string
    [ ANSITerminal.Background Green; ANSITerminal.Bold ]
    (("\nBeginning of day " ^ string_of_int (State.get_index_day st + 1))
    ^ ": "
    ^ Date.date_to_string (State.get_current_day st)
    ^ "\n");
  print_endline "\n";
  print_string
    (ParseRules.get_day_begin_story (State.get_index_day st) ^ "\n")

let locations =
  "data/json/locations.json" |> Yojson.Basic.from_file |> from_json

let pause_game () =
  let _ = prompt_user "Press enter to begin the day\n" in
  ()

let next_days (st : t) =
  let location = rand_elem locations in
  print_endline @@ "\n\n" ^ fst location ^ ":";
  Unix.sleepf 1.;
  print_endline ("\n\n" ^ snd location);
  Unix.sleepf 1.;
  let rec next_days_helper (st : t) (days_left : int) =
    print_beginning_day st;
    pause_game ();
    if days_left = 0 then st
    else
      let new_st = next_day (decr_days_left st) in
      if get_days_left new_st = 0 then new_st
      else next_days_helper new_st (get_days_left new_st)
  in
  let new_st = change_days_left st num_days in
  game_end (next_days_helper new_st (get_days_left new_st))
