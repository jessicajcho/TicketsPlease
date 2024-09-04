type t = {
  incorrect_reason : string;
  day : Date.date;
  index_day : int;
  days_left : int;
  total_num_acc : int;
  total_num_rej : int;
  total_num_arrived : int;
  num_acc : int;
  num_rej : int;
  num_arrived : int;
  correct_guess : int;
  total_correct_guess : int;
  inventory : string list;
  location : string;
  money_gain : int;
  net_money : int;
  total_money_earned : int;
  special_events_trig : bool;
}

let inventory_literal =
  [
    "dog 1";
    "dog 2";
    "food";
    "food";
    "food";
    "water";
    "water";
    "water";
    "dog food";
    "dog food";
    "electric utility";
  ]

let init_state inc_reason =
  {
    incorrect_reason = inc_reason;
    day = Date.make_nonempty_date 03 12 2022;
    index_day = 0;
    days_left = 0;
    num_acc = 0;
    num_rej = 0;
    num_arrived = 0;
    total_num_acc = 0;
    total_num_rej = 0;
    total_num_arrived = 0;
    correct_guess = 0;
    total_correct_guess = 0;
    inventory = inventory_literal;
    location = "Schoellkopf Field";
    money_gain = 0;
    net_money = 0;
    total_money_earned = 0;
    special_events_trig = false;
  }

let advance_day (st : t) =
  { st with day = Date.next_date st.day; index_day = st.index_day + 1 }

let change_days_left st d = { st with days_left = d }
let decr_days_left st = { st with days_left = st.days_left - 1 }
let get_days_left st = st.days_left

let incr_correct_guess (st : t) =
  {
    st with
    correct_guess = st.correct_guess + 1;
    total_correct_guess = st.total_correct_guess + 1;
  }

let change_money (st : t) m = { st with money_gain = st.money_gain + m }

let buy_item (st : t) (c : int) =
  { st with net_money = st.net_money - c }

let update_money (st : t) : t =
  {
    st with
    net_money = st.net_money + st.money_gain;
    total_money_earned = st.net_money + st.money_gain;
    money_gain = 0;
  }

let rec rem_multiples l =
  match l with
  | [] -> []
  | h :: t ->
      if List.exists (fun x -> h == x) t then
        (string_of_int
           (List.length t
           - List.length (List.filter (fun x -> h <> x) t)
           + 1)
        ^ "x " ^ h)
        :: rem_multiples (List.filter (fun x -> h <> x) t)
      else h :: rem_multiples t

let print_inventory (st : t) =
  let inventory = st.inventory in
  "\nHere's your inventory:\n"
  ^
  match inventory with
  | [] -> "None"
  | h :: t ->
      List.fold_left
        (fun x y -> x ^ ", " ^ y)
        h
        (rem_multiples (List.sort compare t))

let manage_accept st is_valid =
  let new_st =
    {
      st with
      num_acc = st.num_acc + 1;
      num_arrived = st.num_arrived + 1;
      total_num_acc = st.total_num_acc + 1;
      total_num_arrived = st.total_num_arrived + 1;
    }
  in
  if is_valid then change_money (incr_correct_guess new_st) 10
  else (
    print_endline ("\n" ^ st.incorrect_reason ^ "\n");
    change_money new_st (-5))

let manage_reject st is_valid =
  let new_st =
    {
      st with
      num_rej = st.num_rej + 1;
      num_arrived = st.num_arrived + 1;
      total_num_rej = st.total_num_rej + 1;
      total_num_arrived = st.total_num_arrived + 1;
    }
  in
  if not is_valid then change_money (incr_correct_guess new_st) 10
  else (
    print_endline ("\n" ^ st.incorrect_reason ^ "\n");
    change_money new_st (-5))

let manage_quit () =
  print_endline "\nThanks for playing! Goodbye.\n";
  exit 0

let rec make_decision (com : Command.command) (st : t) (is_valid : bool)
    =
  match com with
  | Command.Accept _ -> manage_accept st is_valid
  | Command.Reject _ -> manage_reject st is_valid
  | Command.Help -> manage_help st is_valid
  | Command.Inventory -> manage_inventory st is_valid
  | Command.Quit -> manage_quit ()
  | Command.Rules -> manage_rules st is_valid
  | Command.Money -> manage_money st is_valid
  | _ -> manage_other st is_valid

and manage_help st is_valid =
  print_endline (Util.retrieve_string "game_help");
  make_decision
    (Command.parse (TerminalPrint.prompt_user ""))
    st is_valid

and manage_inventory st is_valid =
  print_endline (print_inventory st);
  make_decision
    (Command.parse (TerminalPrint.prompt_user ""))
    st is_valid

and manage_rules st is_valid =
  print_string "\n";
  ignore
    (List.map print_endline (ParseRules.get_day_rules st.index_day));
  make_decision
    (Command.parse (TerminalPrint.prompt_user ""))
    st is_valid

and manage_money st is_valid =
  print_endline
    ("\nYou have $" ^ string_of_int (st.net_money + st.money_gain));
  make_decision
    (Command.parse (TerminalPrint.prompt_user ""))
    st is_valid

and manage_other st is_valid =
  print_endline (Util.retrieve_string "bad_command");
  make_decision
    (Command.parse (TerminalPrint.prompt_user ""))
    st is_valid

let update_location (st : t) (location : string) = { st with location }
let get_current_day (st : t) = st.day
let get_index_day (st : t) = st.index_day

let reset_guesses (st : t) =
  {
    st with
    correct_guess = 0;
    num_acc = 0;
    num_rej = 0;
    num_arrived = 0;
  }

let money_total (st : t) = st.net_money
let money_change (st : t) = st.money_gain

let update_state_reason (st : t) (reason : string) =
  { st with incorrect_reason = reason }

let get_inventory (st : t) = st.inventory

let add_to_inventory (st : t) (item : string) =
  { st with inventory = item :: st.inventory }

let remove_from_inventory (st : t) (item : string) =
  let rec remove_one inv =
    match inv with
    | [] -> []
    | h :: t -> if h = item then t else h :: remove_one t
  in
  { st with inventory = remove_one st.inventory }

let get_guess_stats (st : t) = (st.correct_guess, st.num_arrived)

let get_total_stats (st : t) =
  [
    st.total_num_arrived;
    st.total_num_acc;
    st.total_num_rej;
    st.total_correct_guess;
    st.total_money_earned;
  ]

let get_special_event (st : t) = st.special_events_trig
let get_location (st : t) = st.location
