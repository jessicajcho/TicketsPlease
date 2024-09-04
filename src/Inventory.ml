open State

type item = {
  name : string;
  cost : int;
}

let items_available =
  [
    { name = "water"; cost = 2 };
    { name = "food"; cost = 2 };
    { name = "dog food"; cost = 1 };
    { name = "bang energy drink"; cost = 5 };
    { name = "electric utility"; cost = 5 };
  ]

let item_compute obj_phrase =
  try
    List.find
      (fun item ->
        item.name
        =
        if List.length obj_phrase < 2 then List.hd obj_phrase
        else
          List.fold_left
            (fun a b -> a ^ " " ^ b)
            (List.hd obj_phrase) (List.tl obj_phrase))
      items_available
  with
  | Not_found -> { name = "none"; cost = 0 }
  | Failure _ -> { name = "none"; cost = 0 }

let manage_quit () =
  print_endline "\nThanks for playing! Goodbye.\n";
  exit 0

let rec handle_shop (com : Command.command) (st : t) =
  match com with
  | Command.Continue -> st
  | Command.Buy i ->
      let item = item_compute i in

      if item.name = "none" then handle_none_item st
      else if item.cost <= money_total st then
        handle_legal_purchase st item
      else handle_insufficient_money st
  | Command.Help -> manage_help st
  | Command.Quit -> manage_quit ()
  | Command.Inventory -> manage_inventory st
  | Command.Money -> manage_money st
  | _ -> manage_other st

and manage_help st =
  print_endline (Util.retrieve_string "buy_help");
  handle_shop (Command.parse (TerminalPrint.prompt_user "")) st

and manage_inventory st =
  print_endline (State.print_inventory st);
  handle_shop (Command.parse (TerminalPrint.prompt_user "")) st

and manage_money st =
  print_endline ("\nYou have $" ^ string_of_int @@ money_total st);
  handle_shop (Command.parse (TerminalPrint.prompt_user "")) st

and manage_other st =
  print_endline (Util.retrieve_string "money_explain");
  handle_shop (Command.parse (TerminalPrint.prompt_user "")) st

and handle_none_item st =
  let pr =
    print_endline "There is no such available item. Please try again."
  in
  pr;
  handle_shop (Command.parse (TerminalPrint.prompt_user "")) st

and handle_legal_purchase st item =
  let pr =
    print_endline
      ("You successfully bought " ^ item.name ^ " for $"
     ^ string_of_int item.cost ^ "!")
  in
  pr;
  handle_shop
    (Command.parse (TerminalPrint.prompt_user ""))
    (buy_item (add_to_inventory st item.name) item.cost)

and handle_insufficient_money st =
  let pr =
    print_endline "You can't afford this item. Please try again."
  in
  pr;
  handle_shop (Command.parse (TerminalPrint.prompt_user "")) st

let rec shop_command (st : t) =
  try
    handle_shop
      (Command.parse
         (TerminalPrint.prompt_user "Would you like to buy anything?\n"))
      st
  with
  | Command.Empty ->
      print_endline "\nSorry, this isn't a real command. Try again?";
      shop_command st
  | Command.Malformed ->
      print_endline "\nSorry, this isn't a real command. Try again?";
      shop_command st

let pl_state_helper st has_water has_food =
  if has_water && has_food then (
    print_endline "You consumed one food and one water.\n";
    remove_from_inventory (remove_from_inventory st "water") "food")
  else if has_water then (
    print_endline "You ran out of food and collapsed.\n";
    change_days_left (remove_from_inventory st "water") 0)
  else if has_food then (
    print_endline "You ran out of water and collapsed.\n";
    change_days_left (remove_from_inventory st "food") 0)
  else (
    print_endline "You ran out of food and water and collapsed.\n";
    change_days_left st 0)

let item_exists str st =
  List.exists (fun x -> x = str) (State.get_inventory st)

let tax_and_check_survive st =
  let has_water = item_exists "water" st
  and has_food = item_exists "food" st in
  print_endline "\nYou have arrived back home.\n";
  pl_state_helper (buy_item st 10) has_water has_food

let check_collection st =
  let dog_1 = item_exists "dog 1" st
  and dog_2 = item_exists "dog 2" st in
  if money_total st < 0 then (
    print_endline
      (Util.retrieve_string "fail_tax_collect"
      ^ (if dog_1 && dog_2 then Util.retrieve_string "no_dogs_food"
        else if dog_1 || dog_2 then Util.retrieve_string "no_dog_food"
        else "")
      ^ Util.retrieve_string "kicked_out");
    change_days_left st 0)
  else st

let manage_dog st dog_str =
  let has_dog_food = item_exists "dog food" st in
  if has_dog_food then remove_from_inventory st "dog food"
  else remove_from_inventory st dog_str

let ending_shop_string st dog_1 dog_2 =
  (if dog_1 && dog_2 then Util.retrieve_string "both_dogs"
  else if dog_1 then Util.retrieve_string "dog1"
  else if dog_2 then Util.retrieve_string "dog2"
  else Util.retrieve_string "no_dog")
  ^ Util.retrieve_string "tax_collect"
  ^ string_of_int (money_total st)
  ^ ".\n\n"

let handle_final_state st =
  let dog_1 = item_exists "dog 1" st
  and dog_2 = item_exists "dog 2" st in
  print_endline (ending_shop_string st dog_1 dog_2);
  ANSITerminal.print_string
    [
      ANSITerminal.Background Yellow;
      ANSITerminal.Bold;
      ANSITerminal.Blink;
    ]
    "Shop";
  print_endline (Util.retrieve_string "shop_items");
  shop_command st

let shop (st : t) =
  ANSITerminal.print_string
    [ ANSITerminal.Background Cyan; ANSITerminal.Bold ]
    "Home";
  let pl_st = tax_and_check_survive st in

  let plm_st = check_collection pl_st in

  let d1_st = manage_dog plm_st "dog 1" in

  let d2_st = manage_dog d1_st "dog 2" in
  handle_final_state d2_st
