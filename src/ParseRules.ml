open Yojson.Basic.Util

(*NOTE: if we need to save time here, we can change all structures in
  this module to be arrays. Less lookup time*)

(*[rule_json] is our rules json.contents*)
let rule_json =
  Yojson.Basic.from_file
    ("data" ^ Filename.dir_sep ^ "json" ^ Filename.dir_sep
   ^ "rules.json")

(*[tag_set] represents a tag configuration for a day of who is allowed
  in. Options include:

  age: all, under 21, over 21

  vax: all, 2x, 3x, unvax, 2x+

  role: all, student, faculty, other, cornell

  gender: female, other, male, female/other, all *)

(*NOTES: You can only allow logical vaccination rules (allow everyone,
  allow only 2x and 3x, allow only 3x) *)

type tag_set = {
  age : string;
  gender : string;
  vax : string;
  role : string;
}

(*[rule_day] is a type that contains all information about the rules of
  a day (what functions should be run, which strings should be printed,
  etc.)*)

type rule_day = {
  rules : string list;
  begin_story : string;
  tags : tag_set;
  end_story : string;
}

(*[make_tag_set j] parses a single tag set from a Yojson object [j]*)

let make_tag_set j =
  let tags = to_assoc j in
  {
    age = to_string (List.assoc "age" tags);
    gender = to_string (List.assoc "gender" tags);
    vax = to_string (List.assoc "vax" tags);
    role = to_string (List.assoc "role" tags);
  }

(*[make_rule_day j] parses a single rule_day from the Yojson object
  [j]*)
let make_rule_day j =
  let day_struct = to_assoc j in
  {
    rules = List.map to_string (to_list (List.assoc "rules" day_struct));
    begin_story = to_string (List.assoc "begin_story" day_struct);
    tags = make_tag_set (List.assoc "tags" day_struct);
    end_story = to_string (List.assoc "end_story" day_struct);
  }

(*[rule_day_list] is a list of all rule_days in the json*)
let rule_day_list =
  List.map make_rule_day
    (to_list (List.assoc "dates" (to_assoc rule_json)))

(*[all_rules] is the string list list of all printed rules on all days.
  The length of the outer array in [all_rules] should be the number of
  days in the game. The aim is to compute this to a value immediately
  here so it never needs to be recomputed. *)

let all_rules = List.map (fun x -> x.rules) rule_day_list

(*[all_begin_stories] is the string list of all beginning story strings
  on all days. The length of [all_stories] should be the number of days
  in the game. Aim is to compute only once.*)
let all_begin_stories = List.map (fun x -> x.begin_story) rule_day_list

(*[all_end_stories] is the string list of all ending story strings on
  all days. The length of [all_stories] should be the number of days in
  the game. Aim is to compute only once.*)
let all_end_stories = List.map (fun x -> x.end_story) rule_day_list

(*[all_day_functions] is the tag_set list of all entrance tags to create
  valid docs on all days based on each day's rules. The length of the
  outer list should be the number of days in the game. Days should be 0
  indexed *)

let all_day_tags = List.map (fun x -> x.tags) rule_day_list

let get_day_begin_story (day : int) : string =
  List.nth all_begin_stories day

let get_day_end_story (day : int) : string =
  List.nth all_end_stories day

let get_day_rules (day : int) : string list = List.nth all_rules day
let get_day_tags (day : int) : tag_set = List.nth all_day_tags day
let num_days = List.length rule_day_list

let students_allowed day =
  let tag = get_day_tags day in
  tag.role = "student" || tag.role = "all" || tag.role = "cornell"

let faculty_allowed day =
  let tag = get_day_tags day in
  tag.role = "faculty" || tag.role = "all" || tag.role = "cornell"

let other_role_allowed day =
  let tag = get_day_tags day in
  tag.role = "other" || tag.role = "all"

let under_21_allowed day =
  let tag = get_day_tags day in
  tag.age = "all" || tag.age = "under 21"

let over_21_allowed day =
  let tag = get_day_tags day in
  tag.age = "all" || tag.age = "over 21"

let unvax_allowed day =
  let tag = get_day_tags day in
  tag.vax = "all" || tag.vax = "unvax"

let vax_2x_allowed day =
  let tag = get_day_tags day in
  tag.vax = "all" || tag.vax = "2x" || tag.vax = "2x+"

let vax_3x_allowed day =
  let tag = get_day_tags day in
  tag.vax = "all" || tag.vax = "3x" || tag.vax = "2x+"

let female_allowed day =
  let tag = get_day_tags day in
  tag.gender = "all" || tag.gender = "female"
  || tag.gender = "female/other"

let male_allowed day =
  let tag = get_day_tags day in
  tag.gender = "all" || tag.gender = "male"

let other_gender_allowed day =
  let tag = get_day_tags day in
  tag.gender = "all" || tag.gender = "other"
  || tag.gender = "female/other"
