open TerminalPrint
open Date
open ParseRules

(* [valid_probability] is the probability that a person will have all
   valid documents.*)
let valid_probability = 0.5

(* [id_length] is the length of a person's id. This id can be found on
   school ids and drivers licenses in order to have more to compare.*)
let id_length = 7

(* [eye_colors] is a list of valid eye colors*)
(*Could also nmake this a type, but really we only need strings for
  printing right now *)
let eye_colors = [ "Brown"; "Blue"; "Green"; "Hazel"; "Gray" ]
let main_hair_colors = [ "Blond"; "Brown"; "Black" ]
let secondary_hair_colors = [ "Blue"; "Red"; "Green"; "Turquoise" ]
let tertiary_hair_colors = [ "Gold" ]

type member =
  | Student
  | Faculty
  | OtherMem

type gender =
  | Male
  | Female
  | OtherGen

type person = {
  first_name : string;
  last_name : string;
  dob : Date.date;
  id : string;
  gender : gender;
  role : member;
  family : person list;
  valid_docs : bool;
  height : int;
  eye_color : string;
  hair_color : string;
}

(*[gender_to_string gender] is the string representation of [gender]*)
let gender_to_string gender =
  match gender with
  | Male -> "Male"
  | Female -> "Female"
  | OtherGen -> "Other"

(*[role_to_string role] is the string representation of [role]*)
let role_to_string role =
  match role with
  | Student -> "Student"
  | Faculty -> "Faculty"
  | OtherMem -> "Other"

(*integer in allowed genders allows for efficency if we've already
  calculated for the day (int)*)
let allowed_genders : (int * gender list) ref = ref (-1, [])

(*[compute_allowed_genders ()] updates the allowed genders array each
  day*)
let compute_allowed_genders day =
  let store_day, genders = !allowed_genders in
  if day = store_day then genders
  else
    let genders_result = ref [] in
    if ParseRules.female_allowed day then
      genders_result := Female :: !genders_result;
    if ParseRules.male_allowed day then
      genders_result := Male :: !genders_result;
    if ParseRules.other_gender_allowed day then
      genders_result := OtherGen :: !genders_result;
    allowed_genders := (day, !genders_result);
    let _, recomputed_gender = !allowed_genders in
    recomputed_gender

(*Same idea as allowed_genders but for roles*)
let allowed_roles : (int * member list) ref = ref (-1, [])

let compute_allowed_roles day =
  let store_day, roles = !allowed_roles in
  if store_day = day then roles
  else
    let roles_result = ref [] in
    if ParseRules.students_allowed day then
      roles_result := Student :: !roles_result;
    if ParseRules.faculty_allowed day then
      roles_result := Faculty :: !roles_result;
    if ParseRules.other_role_allowed day then
      roles_result := OtherMem :: !roles_result;
    allowed_roles := (day, !roles_result);
    let _, recomputed_roles = !allowed_roles in
    recomputed_roles

let get_fname person = person.first_name
let get_lname person = person.last_name
let get_dob person = person.dob
let get_id person = person.id
let get_gender person = person.gender
let get_id person = person.id
let get_role person = person.role
let get_family person = person.family
let check_valid_docs person = person.valid_docs
let get_height person = person.height
let get_eye_color person = person.eye_color
let get_hair_color person = person.hair_color

(*[make_gender ()] is a random gender. [make_gender ()] is Female, Male,
  or Other with equal chance.*)
let make_gender day : gender =
  let gender_list = compute_allowed_genders day in
  List.nth gender_list (Random.int (List.length gender_list))

(*[choose_fname g] is a random first name of gender [g]*)
let make_fname (g : gender) =
  let name_lst =
    List.map String.trim
      (if g = Female then
       TerminalPrint.read_from_file "data/fem_other_fnames.csv"
      else if g = Male then
        TerminalPrint.read_from_file "data/male_other_fnames.csv"
      else if Random.bool () then
        TerminalPrint.read_from_file "data/fem_other_fnames.csv"
      else TerminalPrint.read_from_file "data/male_other_fnames.csv")
  in
  let idx = Random.int (List.length name_lst) in
  List.nth name_lst idx

let make_random_fname () =
  let choice_lst = [ Female; Male; OtherGen ] in
  make_fname (List.nth choice_lst (Random.int (List.length choice_lst)))

let make_lname () =
  let lst =
    List.map String.trim
      (TerminalPrint.read_from_file "data/lnames.csv")
  in
  let idx = Random.int (List.length lst) in
  List.nth lst idx

(*[make_dob day_idx date] is a random date of birth that follows the
  rules of [day_idx] based on the current [date].*)
let make_dob day_idx date : Date.date =
  let all_dob_allowed =
    ParseRules.over_21_allowed day_idx
    && ParseRules.under_21_allowed day_idx
  and under_21 = ParseRules.under_21_allowed day_idx
  and over_21 = ParseRules.over_21_allowed day_idx in
  if all_dob_allowed then
    if Random.bool () then Date.make_under_21 date
    else Date.make_over_21 date
  else if under_21 then Date.make_under_21 date
  else if over_21 then Date.make_over_21 date
  else failwith "Must be over or under 21"

(*[make_id_num len] makes a random id number of length [len]. All
  characters in the output id are digits 0-9*)
let make_id_num (len : int) : string =
  let rec construct_id_num (len_to_go : int) (acc : string) : string =
    match len_to_go with
    | 0 -> acc
    | n ->
        if n > 0 then
          construct_id_num (n - 1) (acc ^ string_of_int (Random.int 10))
        else failwith "Not a valid ID length: Negative."
  in
  let acc = "" in
  construct_id_num len acc

(*[make_eye_color ()] is an eye color chosen randomly from
  [eye_colors]*)
let make_eye_color () =
  let idx = Random.int (List.length eye_colors) in
  List.nth eye_colors idx

let make_hair_color () =
  let list = Random.int 10 in
  if list < 7 then
    List.nth main_hair_colors
      (Random.int (List.length main_hair_colors))
  else if list <= 8 then
    List.nth secondary_hair_colors
      (Random.int (List.length secondary_hair_colors))
  else
    List.nth tertiary_hair_colors
      (Random.int (List.length tertiary_hair_colors))

(*[make_height_cm ()] is a height in centimeters. A height can be
  between 4 and 7 feet.*)
let make_height_cm () = Random.int 91 + 122

let set_gender g =
  let gender_string = String.lowercase_ascii g in
  match gender_string with
  | "m" | "male" -> Male
  | "f" | "female" -> Female
  | "o" | "other" -> OtherGen
  | _ -> failwith "Invalid gender string input"

let set_role r =
  let role_string = String.lowercase_ascii r in

  match role_string with
  | "student" -> Student
  | "faculty" -> Faculty
  | "other" -> OtherMem
  | _ -> failwith "Invalid role string"

(*[has_valid_docs] is [true] with probability [valid_probability] and
  [false] otherwise. This function is useful for randomly determining if
  a person has valid documentation.*)
let has_valid_docs () =
  if Random.float 1.0 <= valid_probability then true else false

(*[make_role dob] is a random role for someone with date of birth [dob].
  If [dob] implies someone is not yet 27 as of 2022, they cannot be a
  faculty member.*)

let make_role day (dob : Date.date) : member =
  let usable_roles =
    if Date.get_year dob <= 1994 then compute_allowed_roles day
    else List.filter (fun x -> x <> Faculty) (compute_allowed_roles day)
  in
  List.nth usable_roles (Random.int (List.length usable_roles))

let make_person day_idx date =
  let g = make_gender day_idx and dob = make_dob day_idx date in

  {
    first_name = make_fname g;
    last_name = make_lname ();
    dob;
    id = make_id_num id_length;
    gender = g;
    role = make_role day_idx dob;
    family = [];
    valid_docs = has_valid_docs ();
    height = make_height_cm ();
    eye_color = make_eye_color ();
    hair_color = make_hair_color ();
  }
