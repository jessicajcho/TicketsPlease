open Date
open TerminalPrint
open Yojson.Basic.Util

(*num_chars_to_concat: number of characters that need to be appended in
  the concat, some may be " "*)
type print_item = {
  num_chars_to_concat : int;
  print_val : string;
}

let vaccine_json =
  Yojson.Basic.from_file
    ("data" ^ Filename.dir_sep ^ "json" ^ Filename.dir_sep
   ^ "vaccine_format.json")

let license_json =
  Yojson.Basic.from_file
    ("data" ^ Filename.dir_sep ^ "json" ^ Filename.dir_sep
   ^ "license_format.json")

let school_id_json =
  Yojson.Basic.from_file
    ("data" ^ Filename.dir_sep ^ "json" ^ Filename.dir_sep
   ^ "school_id_format.json")

let parse_inner_record j =
  let inner_record = to_assoc j in
  {
    num_chars_to_concat =
      to_int (List.assoc "num_chars_to_concat" inner_record);
    print_val = to_string (List.assoc "print_val" inner_record);
  }

let parse_inner_lst j =
  let inner_lst = to_list j in
  List.map parse_inner_record inner_lst

let vaccine_card_format : print_item list list =
  let raw_format = to_assoc vaccine_json |> List.assoc "format" in
  let outer_lst = to_list raw_format in
  List.map parse_inner_lst outer_lst

let school_id_format : print_item list list =
  let raw_format = to_assoc school_id_json |> List.assoc "format" in
  let outer_lst = to_list raw_format in
  List.map parse_inner_lst outer_lst

let drivers_license_format : print_item list list =
  let raw_format = to_assoc license_json |> List.assoc "format" in
  let outer_lst = to_list raw_format in
  List.map parse_inner_lst outer_lst

(* input documents as string lists -> each line is a list of inputs on
   that line*)

(*put formats here*)

let combine_print_item_to_string (s : string) (p1 : print_item) : string
    =
  s ^ p1.print_val

let rec format_line
    (line_fmt : print_item list)
    (values : string list)
    (acc : string) : string =
  match (line_fmt, values) with
  | [], [] -> acc
  | [], _ :: _ -> failwith "Too many inputs print into doc line"
  | l, [] -> acc ^ List.fold_left combine_print_item_to_string "" l
  | h1 :: t1, str_input :: t2 ->
      handle_full_format acc h1 t1 str_input t2

and handle_full_format acc h1 t1 str_input t2 =
  let max_padding = h1.num_chars_to_concat in
  assert (max_padding >= String.length str_input);
  let padding =
    str_input ^ String.make (max_padding - String.length str_input) ' '
  in
  format_line t1 t2 (acc ^ h1.print_val ^ padding)

(*O(n^2) kinda, not great *)
let rec format_document
    (fmt : print_item list list)
    (values : string list list)
    lst : string list =
  match (fmt, values) with
  | [], [] -> lst
  | [], _ :: _ -> failwith "Too many input values to docprint"
  | _ :: _, [] -> failwith "Not enough input values to docprint"
  | h1 :: t1, h2 :: t2 ->
      format_document t1 t2 (lst @ [ format_line h1 h2 "" ])

(*[vax_triple] holds a triplet of dates to populate a vaccine card with
  . Created because it's much easier to access than pattern matching
  multiple times.*)
type vax_triple = {
  dose1 : Date.date;
  dose2 : Date.date;
  booster : Date.date;
}

let allow_all_vax day =
  ParseRules.unvax_allowed day
  && ParseRules.vax_2x_allowed day
  && ParseRules.vax_3x_allowed day

let allow_boosted day =
  ParseRules.vax_3x_allowed day
  && (not (ParseRules.vax_2x_allowed day))
  && not (ParseRules.unvax_allowed day)

(*[increase_vax_option day dates] alters a fully boosted vaccination
  card with [dates] to make legal vaccination cards based on index day
  [day].*)

let legalize_vax_option day dates =
  let choice = Random.float 1.
  and empty_date = Date.make_nonempty_date 0 0 0 in

  if allow_all_vax day then
    if choice <= 0.33 then
      { dates with dose2 = empty_date; booster = empty_date }
    else if choice <= 0.66 then { dates with booster = empty_date }
    else dates
  else if ParseRules.vax_2x_allowed day && ParseRules.vax_3x_allowed day
  then
    if choice <= 0.5 then { dates with booster = empty_date } else dates
  else if allow_boosted day then dates
  else failwith "Uncovered vaccination case"

let make_vax_dates day (current_date : Date.date) =
  let days_since_dose1 = Random.int 183 + 210 in
  let dose1_date =
    Date.reverse_multiple_days current_date days_since_dose1
  in
  let dose2_date =
    Date.advance_multiple_days dose1_date (Random.int 7 + 14)
  in
  let booster = Date.advance_multiple_days dose2_date 152 in
  legalize_vax_option day
    { dose1 = dose1_date; dose2 = dose2_date; booster }

type covid_vaccination_card = {
  first_name : string;
  last_name : string;
  dob : Date.date;
  dose1 : Date.date;
  dose2 : Date.date;
  booster : Date.date;
}

(*I'm going to say someone's school ID number and driver's license ID
  number is the same as their person ID so we have more things to
  compare. If someone has a better idea, feel free to implement.*)

type school_id = {
  netid : string;
  cornellid : string;
  first_name : string;
  last_name : string;
  role : Person.member;
  expiration : Date.date;
}

type drivers_license = {
  first_name : string;
  last_name : string;
  dob : Date.date;
  id : string;
  expiration_date : Date.date;
  gender : Person.gender;
  height : int;
  eye_color : string;
  hair_color : string;
}

(* A type to connect all documents so we can use them inside the same
   functions without rewriting*)

type doc_type =
  | VaccineCard of covid_vaccination_card
  | SchoolID of school_id
  | DriversLicense of drivers_license
  | None

(*[make_covid_vax_card current_date p] is the COVID vaccine card for
  person [p] who arrives on date [current_date].*)

let make_covid_vax_card
    (day : int)
    (current_date : Date.date)
    (p : Person.person) : doc_type =
  let vax_dates = make_vax_dates day current_date in
  VaccineCard
    {
      first_name = Person.get_fname p;
      last_name = Person.get_lname p;
      dob = Person.get_dob p;
      dose1 = vax_dates.dose1;
      dose2 = vax_dates.dose2;
      booster = vax_dates.booster;
    }

(*[make_netid p] is a netid for person [p]*)
let make_netid (p : Person.person) : string =
  let has_2_nums = Random.bool ()
  and beginning =
    String.make 1 (Char.lowercase_ascii (Person.get_fname p).[0])
    ^ String.make 1 (Char.lowercase_ascii (Person.get_lname p).[0])
    ^ string_of_int (Random.int 10)
    ^ string_of_int (Random.int 10)
  in
  if has_2_nums then beginning
  else beginning ^ string_of_int (Random.int 10)

let make_expiration_date current_date : Date.date =
  Date.advance_multiple_days current_date (Random.int 229 + 1)

(*[make_school_id p] is a Cornell id for person [p]*)
let make_school_id (current_date : Date.date) (person : Person.person) :
    doc_type =
  SchoolID
    {
      netid = make_netid person;
      cornellid = Person.get_id person;
      first_name = Person.get_fname person;
      last_name = Person.get_lname person;
      role = Person.get_role person;
      expiration = make_expiration_date current_date;
    }

(*[make_expiration_date current_date)] is a valid expiration date, which
  cannot be before the [current_date]*)

(*[make_drivers_license current_date person] is a driver's license for
  person [p] as of date [d].*)
let make_drivers_license
    (current_date : Date.date)
    (person : Person.person) : doc_type =
  DriversLicense
    {
      first_name = Person.get_fname person;
      last_name = Person.get_lname person;
      dob = Person.get_dob person;
      id = Person.get_id person;
      expiration_date = make_expiration_date current_date;
      gender = Person.get_gender person;
      height = Person.get_height person;
      eye_color = Person.get_eye_color person;
      hair_color = Person.get_hair_color person;
    }

type documents = {
  vaccine : doc_type;
  id : doc_type;
  license : doc_type;
}

let make_valid_documents
    (day : int)
    (current_date : Date.date)
    (person : Person.person) : documents =
  {
    vaccine = make_covid_vax_card day current_date person;
    id = make_school_id current_date person;
    license = make_drivers_license current_date person;
  }

(* Add a reset documents here to input a doctype (newly incorrectified)
   + output the modified set of docs like make _valid_doc*)

let get_vaccine_card docs = docs.vaccine
let get_id_card docs = docs.id
let get_license docs = docs.license

(*We should determine what's best to do in failing cases later *)
let get_fname (doc : doc_type) : string =
  match doc with
  | VaccineCard t -> t.first_name
  | SchoolID t -> t.first_name
  | DriversLicense t -> t.first_name
  | None -> failwith "No first name, NoneType document"

let get_lname (doc : doc_type) : string =
  match doc with
  | VaccineCard t -> t.last_name
  | SchoolID t -> t.last_name
  | DriversLicense t -> t.last_name
  | None -> failwith "No first name, NoneType document"

let get_id (doc : doc_type) : string =
  match doc with
  | VaccineCard _ -> failwith "No ID number, vaccine card"
  | None -> failwith "No ID number, None document"
  | SchoolID t -> t.cornellid
  | DriversLicense t -> t.id

let get_dob (doc : doc_type) : string =
  match doc with
  | None -> failwith "No document, no DOB"
  | VaccineCard t -> Date.date_to_string t.dob
  | DriversLicense t -> Date.date_to_string t.dob
  | SchoolID _ -> failwith "No DOB, SchoolID"

let get_gender (doc : doc_type) : string =
  match doc with
  | None -> failwith "No document, no gender"
  | VaccineCard t -> "No gender, vaccine card"
  | DriversLicense t -> Person.gender_to_string t.gender
  | SchoolID _ -> failwith "No gender, SchoolID"

let get_eye_color (doc : doc_type) : string =
  match doc with
  | None -> failwith "No eye color, None document"
  | VaccineCard _ -> failwith "No eye color, vaccine card"
  | DriversLicense t -> t.eye_color
  | SchoolID _ -> failwith "No eye color, SchoolID"

let get_hair_color (doc : doc_type) : string =
  match doc with
  | None -> failwith "No hair color, None document"
  | VaccineCard _ -> failwith "No hair color, vaccine card"
  | DriversLicense t -> t.hair_color
  | SchoolID _ -> failwith "No hair color, SchoolID"

let get_height (doc : doc_type) : int =
  match doc with
  | None -> failwith "No height, None document"
  | VaccineCard _ -> failwith "No height, vaccine card"
  | SchoolID _ -> failwith "No height, SchoolID"
  | DriversLicense t -> t.height

let get_dose_1 (doc : doc_type) : string =
  match doc with
  | None -> failwith "No dose 1, None document"
  | VaccineCard t -> Date.date_to_string t.dose1
  | SchoolID _ -> failwith "No dose 1, SchoolID"
  | DriversLicense _ -> failwith "No dose 1, Driver's License"

let get_dose1_date (doc : doc_type) : Date.date =
  match doc with
  | None -> failwith "No dose 1, None document"
  | VaccineCard t -> t.dose1
  | SchoolID _ -> failwith "No dose 1, SchoolID"
  | DriversLicense _ -> failwith "No dose 1, Driver's License"

let get_dose2_date (doc : doc_type) : Date.date =
  match doc with
  | None -> failwith "No dose 1, None document"
  | VaccineCard t -> t.dose2
  | SchoolID _ -> failwith "No dose 1, SchoolID"
  | DriversLicense _ -> failwith "No dose 1, Driver's License"

let get_dose_2 (doc : doc_type) : string =
  match doc with
  | None -> failwith "No dose 2, None document"
  | VaccineCard t -> Date.date_to_string t.dose2
  | SchoolID _ -> failwith "No dose 2, SchoolID"
  | DriversLicense _ -> failwith "No dose 2, Driver's License"

let get_booster (doc : doc_type) : string =
  match doc with
  | None -> failwith "No booster, None document"
  | VaccineCard t -> Date.date_to_string t.booster
  | SchoolID _ -> failwith "No booster, SchoolID"
  | DriversLicense _ -> failwith "No booster, Driver's License"

let get_expiration (doc : doc_type) : string =
  match doc with
  | None -> failwith "No expiration, None document"
  | DriversLicense t -> Date.date_to_string t.expiration_date
  | SchoolID s -> Date.date_to_string s.expiration
  | VaccineCard _ -> failwith "No expiration, VaccineCard"

let get_expiration_date doc : Date.date =
  match doc with
  | None -> failwith "No expiration, None document"
  | DriversLicense t -> t.expiration_date
  | SchoolID s -> s.expiration
  | VaccineCard _ -> failwith "No expiration, VaccineCard"

let get_netid (doc : doc_type) : string =
  match doc with
  | None -> failwith "No netid, None document"
  | DriversLicense t -> "No netid, Driver's License"
  | SchoolID t -> t.netid
  | VaccineCard _ -> failwith "No netid, VaccineCard"

let get_role (id : doc_type) =
  match id with
  | None -> failwith "No role, None document"
  | DriversLicense t -> "No role, Driver's License"
  | SchoolID t -> Person.role_to_string t.role
  | VaccineCard _ -> failwith "No role, VaccineCard"

let remake_doc_fname doc fname =
  match doc with
  | VaccineCard t -> VaccineCard { t with first_name = fname }
  | SchoolID t -> SchoolID { t with first_name = fname }
  | DriversLicense t -> DriversLicense { t with first_name = fname }
  | None -> None

let remake_doc_lname doc lname =
  match doc with
  | VaccineCard t -> VaccineCard { t with last_name = lname }
  | SchoolID t -> SchoolID { t with last_name = lname }
  | DriversLicense t -> DriversLicense { t with last_name = lname }
  | None -> None

let remake_doc_id doc id =
  match doc with
  | VaccineCard _ -> failwith "No ID in Vaccine Card"
  | SchoolID t -> SchoolID { t with cornellid = id }
  | DriversLicense t -> DriversLicense { t with id }
  | None -> None

let remake_doc_netid doc netid =
  match doc with
  | VaccineCard _ -> failwith "No netid in Vaccine Card"
  | SchoolID t -> SchoolID { t with netid }
  | DriversLicense _ -> failwith "No netid in Driver's License"
  | None -> None

let remake_doc_expiration doc exp =
  match doc with
  | VaccineCard _ -> failwith "No expiration in Vaccine Card"
  | SchoolID _ -> failwith "No expiration in School ID"
  | DriversLicense t -> DriversLicense { t with expiration_date = exp }
  | None -> None

let make_none_doc () = None

let remake_doc_gender doc g =
  match doc with
  | VaccineCard _ -> failwith "No gender in Vaccine Card"
  | SchoolID _ -> failwith "No Gender in School ID"
  | DriversLicense t -> DriversLicense { t with gender = g }
  | None -> None

let remake_doc_role doc r =
  match doc with
  | VaccineCard _ -> failwith "No role in Vaccine Card"
  | SchoolID t -> SchoolID { t with role = r }
  | DriversLicense _ -> failwith "No role in Driver's License"
  | None -> None

let remake_doc_dob doc dob =
  match doc with
  | VaccineCard t -> VaccineCard { t with dob }
  | DriversLicense t -> DriversLicense { t with dob }
  | SchoolID _ -> failwith "No DOB on School ID"
  | None -> None

let remake_vax_dates doc dose1 dose2 booster =
  match doc with
  | VaccineCard t -> VaccineCard { t with dose1; dose2; booster }
  | DriversLicense t ->
      failwith "No vaccination dates on Driver's License"
  | SchoolID _ -> failwith "No vaccination dates on School ID"
  | None -> None

let swap_vax_dates doc =
  match doc with
  | VaccineCard t ->
      VaccineCard { t with dose1 = t.dose2; dose2 = t.dose1 }
  | DriversLicense _ ->
      failwith "No vax dates to swap in Driver's License"
  | SchoolID _ -> failwith "No vax dates to swap in School ID"
  | None -> None

let get_doc_gender doc =
  match doc with
  | VaccineCard _ -> failwith "No gender on Vaccine Card"
  | DriversLicense t -> t.gender
  | SchoolID _ -> failwith "No gender on School ID"
  | None -> failwith "No gender on empty document"

let reset_docs_vaccine_card docs vax = { docs with vaccine = vax }
let reset_docs_school_id docs id = { docs with id }
let reset_docs_license docs license = { docs with license }

let fill_school_id_inputs id =
  [
    [];
    [];
    [];
    [ get_fname id; get_lname id ];
    [ get_netid id ];
    [ get_id id ];
    [ get_role id ];
    [ get_expiration id ];
    [];
  ]

let make_school_id_inputs (id : doc_type) : string list list =
  match id with
  | None -> failwith "Incorrect document type"
  | VaccineCard _ -> failwith "Incorrect document type"
  | DriversLicense _ -> failwith "Incorrect document type"
  | SchoolID t -> fill_school_id_inputs id

let fill_vaccine_card_inputs id =
  [
    [];
    [];
    [];
    [ get_fname id; get_lname id ];
    [ get_dob id ];
    [ get_dose_1 id ];
    [ get_dose_2 id ];
    [ get_booster id ];
    [];
  ]

let make_vaccine_card_inputs (id : doc_type) : string list list =
  match id with
  | None -> failwith "Incorrect document type"
  | VaccineCard _ -> fill_vaccine_card_inputs id
  | DriversLicense _ -> failwith "Incorrect document type"
  | SchoolID t -> failwith "Incorrect document type"

let fill_license_inputs id =
  [
    [];
    [];
    [];
    [ get_fname id; get_lname id ];
    [ get_dob id; get_gender id ];
    [ get_id id ];
    [ get_eye_color id ];
    [ get_hair_color id ];
    [ get_expiration id ];
    [];
  ]

let make_drivers_license_inputs (id : doc_type) : string list list =
  match id with
  | None -> failwith "Incorrect document type"
  | VaccineCard _ -> failwith "Incorrect document type"
  | DriversLicense _ -> fill_license_inputs id
  | SchoolID t -> failwith "Incorrect document type"

let print_school_id (school_id : doc_type) =
  match school_id with
  | None -> print_string ""
  | DriversLicense _ ->
      failwith
        "Do not use {|print_school_id|} to print driver's license."
  | SchoolID t ->
      print_list
        (format_document school_id_format
           (make_school_id_inputs school_id)
           [])
  | VaccineCard _ ->
      failwith "Do not use {|print_school_id|} to print a vaccine card."

let print_vaccine_card (vax_card : doc_type) =
  match vax_card with
  | None -> print_string ""
  | DriversLicense _ ->
      failwith
        "Do not use {|print_vaccine_card|} to print driver's license."
  | SchoolID t ->
      failwith "Do not use {|print_vaccine_card|} to print a school id."
  | VaccineCard _ ->
      print_list
        (format_document vaccine_card_format
           (make_vaccine_card_inputs vax_card)
           [])

let print_drivers_license (drivers_license : doc_type) =
  match drivers_license with
  | None -> print_string ""
  | DriversLicense _ ->
      print_list
        (format_document drivers_license_format
           (make_drivers_license_inputs drivers_license)
           [])
  | SchoolID t ->
      failwith
        "Do not use {|print_drivers_license|} to print school id."
  | VaccineCard _ ->
      failwith
        "Do not use {|print_drivers_license|} to print a vaccine card."

let print_documents (docs : documents) =
  print_school_id (get_id_card docs);
  print_drivers_license (get_license docs);
  print_vaccine_card (get_vaccine_card docs)
