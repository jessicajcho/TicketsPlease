open OUnit2
open Game
open Date
open Util
open Command
open Person
open State

(* TEST PLAN:

   We created OUnit test cases for the modules that were testable:
   Util.ml, State.ml, Person.ml, Command.ml, and Date.ml. Each set of
   module tests is clearly commented in a section of our testing file.
   The remaining modules were manually tested because they often relied
   heavily on the previously mentioned modules (that were tested in the
   OUnit suite), mainly had functions that only formatted/printed
   strings, or mostly contained functions that randomized their outputs.
   For instance, Documents.ml simply takes the values from Person.ml to
   format a person for printing, and InvalidDoc.ml makes completely
   randomized outputs based on what is in Documents.ml. All functions in
   TerminalPrint.ml and TitleScreen.ml are only used to print to the
   screen. Thus, these modules are more effectively tested via manual
   testing because 1) they are impossible to isolate in testing, 2) have
   random function output, 3) commonly print to the screen, and 4) it is
   immediately evident (visible) if one of these modules has an issue.
   We can also manually increase the probability of certain outputs
   occurring, which allowed us to manually test for more specific cases.
   For our OUnit test cases, we used mostly black box testing, using the
   specification to guide the creation of the test cases; this involved
   making typical cases, along with trying edge cases (such as empty
   string, negative values, 0, empty list, etc.) to ensure that no
   unexpected exceptions were raised. Through this approach, we were
   able to ensure that our program was correct because we thoroughly
   tested many functions using both typical and boundary OUnit test
   cases, and manually tested out all other functions by playing the
   game many times throughout the development of the program. *)

(*********** Date.ml Tests ***********)
let date_exists_test
    (name : string)
    (date : Date.date)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal (date_exists date) expected_output
    ~printer:string_of_bool

let next_date_test
    (name : string)
    (date : Date.date)
    (expected_output : Date.date) =
  name >:: fun _ ->
  assert_equal (next_date date) expected_output ~printer:date_to_string

let advance_mul_days_test
    (name : string)
    (date : Date.date)
    (num_days : int) =
  let advance_result = advance_multiple_days date num_days in
  let reverse_result =
    Date.reverse_multiple_days advance_result num_days
  in
  name >:: fun _ ->
  assert_equal reverse_result date ~printer:date_to_string

let reverse_mul_yrs_test
    (name : string)
    (date : Date.date)
    (num_yrs : int)
    (expected_output : Date.date) =
  name >:: fun _ ->
  assert_equal
    (reverse_multiple_years date num_yrs)
    expected_output ~printer:date_to_string

let date_tests =
  let empty = make_empty_date () in
  let d1 = make_nonempty_date 10 5 2000 in
  let d1' = make_nonempty_date 11 5 2000 in
  let d1_rev = make_nonempty_date 10 5 1997 in
  let d2 = make_nonempty_date 31 12 2012 in
  let d2' = make_nonempty_date 01 1 2013 in
  let d2_rev = make_nonempty_date 31 12 2002 in
  [
    date_exists_test "date exists" d1 true;
    date_exists_test "date doesn't exist" empty false;
    next_date_test "5/10/2000->5/11/2000" d1 d1';
    next_date_test "12/13/2012->1/01/2013" d2 d2';
    advance_mul_days_test "5/10/2000 advance 1" d1 1;
    advance_mul_days_test "12/31/2012 advance 10" d2 10;
    reverse_mul_yrs_test "5/10/2000 rev 3 yrs" d1 3 d1_rev;
    reverse_mul_yrs_test "1/01/2003 rev 10 yrs" d2 10 d2_rev;
  ]

(**** Util.ml tests ****)

let explode_test (name : string) (input : string) =
  let exploded = explode input in
  name >:: fun _ -> assert_equal (implode exploded) input

let replace_string_char_test
    (name : string)
    (str : string)
    (index : int)
    (c : char)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal
    (replace_string_char str index c)
    expected_output ~printer:String.escaped

let util_tests =
  [
    explode_test "explode \"the Cat\"" "the Cat";
    explode_test "explode empty string" "";
    replace_string_char_test "replace index 4 with l" "the dogs" 4 'l'
      "the logs";
    replace_string_char_test "replace index -1 (invalid)" "waterbottle"
      (-1) 'l' "waterbottle";
    replace_string_char_test "replace index 7 (invalid)" "pencil" 7 'l'
      "pencil";
  ]

(*** Command.ml tests ***)
let parse_test
    (name : string)
    (str : string)
    (expected_output : command) =
  name >:: fun _ -> assert_equal (parse str) expected_output

let invalid_parse_test (name : string) (str : string) =
  name >:: fun _ -> assert_raises Malformed (fun () -> parse str)

let command_tests =
  [
    parse_test "continue" "continue" Continue;
    parse_test "accept with object" "accept the person"
      (Accept [ "the"; "person" ]);
    parse_test "accept empty object" "accept" (Accept []);
    parse_test "reject with object" "reject reject"
      (Reject [ "reject" ]);
    parse_test "reject empty object" "reject" (Reject []);
    invalid_parse_test "invalid command" "accepts";
    invalid_parse_test "invalid command" "blah blah []";
  ]

(*** Person.ml tests ***)
let possible_genders =
  [
    "m";
    "M";
    "male";
    "Male";
    "f";
    "F";
    "female";
    "Female";
    "o";
    "O";
    "other";
    "Other";
  ]

let possible_hair =
  [
    "Blond";
    "Brown";
    "Black";
    "Blue";
    "Red";
    "Green";
    "Turquoise";
    "Gold";
  ]

let possible_eye = [ "Brown"; "Blue"; "Green"; "Hazel"; "Gray" ]
let possible_roles = [ "Student"; "Faculty"; "Other" ]

let get_gender_test (name : string) (p : person) =
  let gender = gender_to_string @@ get_gender p in
  name >:: fun _ -> assert_bool "" (List.mem gender possible_genders)

let get_color_test (name : string) (p : person) =
  let hair_col = get_hair_color p in
  let eye_col = get_eye_color p in
  name >:: fun _ ->
  assert_bool ""
    (List.mem hair_col possible_hair && List.mem eye_col possible_eye)

let get_height_test (name : string) (p : person) =
  name >:: fun _ -> assert_bool "" (get_height p > 0)

let get_name_test (name : string) (p : person) =
  name >:: fun _ ->
  assert_bool "" (get_fname p <> "" && get_lname p <> "")

let get_dob_test (name : string) (p : person) =
  name >:: fun _ -> assert_equal (get_dob p |> date_exists) true

let get_role_test (name : string) (p : person) =
  name >:: fun _ ->
  assert_bool ""
    (List.mem (get_role p |> role_to_string) possible_roles)

let person_tests =
  let d1 = make_nonempty_date 10 5 2000 in
  let d2 = make_nonempty_date 01 07 1978 in
  let p1 = make_person 1 d1 in
  let p2 = make_person 2 d2 in
  [
    get_gender_test "get gender test for person1" p1;
    get_gender_test "get gender test for person2" p2;
    get_color_test "get hair/eye color test for person1" p1;
    get_color_test "get hair/eye color test for person2" p2;
    get_height_test "height of person1" p1;
    get_height_test "height of person1" p2;
    get_name_test "name of person1" p1;
    get_name_test "name of person2" p2;
    get_dob_test "dob of person1" p1;
    get_dob_test "dob of person2" p2;
    get_role_test "role of person1" p1;
    get_role_test "role of person2" p2;
  ]

(*** State.ml tests ***)
let advance_day_test (name : string) (st : State.t) =
  let orig_index = get_index_day st in
  let new_st = advance_day st in
  name >:: fun _ ->
  assert_equal (orig_index + 1) (get_index_day new_st)
    ~printer:string_of_int

let change_days_left_test
    (name : string)
    (new_days : int)
    (st : State.t) =
  let new_st = change_days_left st new_days in
  name >:: fun _ -> assert_equal new_days (get_days_left new_st)

let decr_days_left_test (name : string) (st : State.t) =
  let orig_days = get_days_left st in
  let new_days = decr_days_left st |> get_days_left in
  name >:: fun _ -> assert_equal new_days (orig_days - 1)

let reset_guesses_test (name : string) (st : State.t) =
  let reset = reset_guesses st in
  let guess_stats = get_guess_stats reset in
  name >:: fun _ -> assert_equal guess_stats (0, 0)

let buy_item_test (name : string) (st : State.t) (cost : int) =
  let orig_net = money_total st in
  let new_st = buy_item st cost in
  name >:: fun _ -> assert_equal (orig_net - cost) (money_total new_st)

let add_invent_test (name : string) (st : State.t) (item : string) =
  let new_st_invent = add_to_inventory st item |> get_inventory in
  name >:: fun _ -> assert_bool "" (List.mem item new_st_invent)

let remove_invent_test (name : string) (st : State.t) (item : string) =
  let new_st1 = remove_from_inventory st item in
  let new_st2_invt = add_to_inventory new_st1 item |> get_inventory in
  name >:: fun _ ->
  assert_equal
    (List.length new_st2_invt)
    (st |> get_inventory |> List.length)

let update_loc_test (name : string) (st : State.t) (loc : string) =
  let new_st = update_location st loc in
  name >:: fun _ -> assert_equal (get_location new_st) loc

let update_money_test (name : string) (st : State.t) (money : int) =
  let orig_net = money_total st in
  let new_st = change_money st money |> update_money in
  name >:: fun _ -> assert_equal (orig_net + money) (money_total new_st)

let state_tests =
  let st_0 = init_state "" in
  let st_1 = advance_day st_0 in
  [
    advance_day_test "advance day 0 -> 1" st_0;
    advance_day_test "advance day 1 -> 2" st_1;
    change_days_left_test "change days to -1" (-1) st_0;
    change_days_left_test "change days to current (no change)"
      (get_days_left st_1) st_1;
    decr_days_left_test "decr days for init state" st_0;
    decr_days_left_test "decr days for state 1" st_1;
    reset_guesses_test "reset guesses" st_0;
    buy_item_test "buy free item" st_0 0;
    buy_item_test "buy item with negative cost" st_0 (-10);
    buy_item_test "buy $5 item" st_0 5;
    add_invent_test "add empty" st_0 "";
    add_invent_test "add water" st_0 "water";
    remove_invent_test "remove then add water" st_0 "water";
    update_loc_test "update location to paris" st_1 "paris";
    update_loc_test "update location to empty" st_1 "";
    update_money_test "update money with $0" st_0 0;
    update_money_test "update money with $(-50)" st_0 (-50);
    update_money_test "update money with $100" st_0 100;
  ]

let suite =
  "test suite"
  >::: List.flatten
         [
           date_tests;
           util_tests;
           command_tests;
           person_tests;
           state_tests;
         ]

let _ = run_test_tt_main suite
