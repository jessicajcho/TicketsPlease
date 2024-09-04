open Util

(*This module will include all of the ways we can "incorrectify"
  documents. *)

(*[resample f eq] resamples for a value until the value chosen is not
  [eq]. [f] is the sampling function and must input only a unit ()
  parameter. *)

let rec resample f eq =
  let sample = f () in
  if sample = eq then resample f eq else sample

(*[disallowed_genders] is contains the current valid date and the list
  of disallowed genders for this date. This list could possibly be
  empty. ACCESS ONLY VIA COMPUTATION*)
let disallowed_genders : (int * Person.gender list) ref = ref (-1, [])

let compute_disallowed_genders day =
  let store_day, gen_list = !disallowed_genders in
  if store_day = day then gen_list
  else
    let new_gen_list = ref [] in
    if not (ParseRules.female_allowed day) then
      new_gen_list := Person.set_gender "female" :: !new_gen_list;
    if not (ParseRules.male_allowed day) then
      new_gen_list := Person.set_gender "male" :: !new_gen_list;
    if not (ParseRules.other_gender_allowed day) then
      new_gen_list := Person.set_gender "other" :: !new_gen_list;
    disallowed_genders := (day, !new_gen_list);
    let _, return_genders = !disallowed_genders in
    return_genders

(*[disallowed_roles] is contains the current valid date and the list of
  disallowed roles for this date. This list could possibly be empty

  ACCESS ONLY VIA COMPUTATION*)
let disallowed_roles : (int * Person.member list) ref = ref (-1, [])

let compute_disallowed_roles day =
  let store_day, role_list = !disallowed_roles in
  if store_day = day then role_list
  else
    let new_role_list = ref [] in
    if not (ParseRules.female_allowed day) then
      new_role_list := Person.set_role "student" :: !new_role_list;
    if not (ParseRules.male_allowed day) then
      new_role_list := Person.set_role "faculty" :: !new_role_list;
    if not (ParseRules.other_role_allowed day) then
      new_role_list := Person.set_role "other" :: !new_role_list;
    disallowed_roles := (day, !new_role_list);
    let _, return_roles = !disallowed_roles in
    return_roles

(*[vax_status_type] is a named record to see which vaccination statuses
  are not allowed. (True is NOT allowed)*)
type vax_status_type = {
  unvax : bool;
  double : bool;
  boosted : bool;
}

(*[disallowed_vax_status] contains the current list of disallowed
  vaccination states.

  ACCESS ONLY VIA COMPUTATION*)
let disallowed_vax_status : (int * vax_status_type) ref =
  ref (-1, { unvax = false; double = false; boosted = false })

let compute_disallowed_vaccinations day =
  let store_day, vax_list = !disallowed_vax_status in
  if store_day = day then vax_list
  else (
    disallowed_vax_status :=
      ( day,
        {
          unvax = not (ParseRules.unvax_allowed day);
          double = not (ParseRules.vax_2x_allowed day);
          boosted = not (ParseRules.vax_3x_allowed day);
        } );
    let _, new_status = !disallowed_vax_status in
    new_status)

(*[age_status] is a type holding the disallowed age statuses. TRUE if an
  age is NOT allowed*)

type age_status = {
  over_21 : bool;
  under_21 : bool;
}

(*[disallow_age_status] contains the currently disallowed ages. DO NOT
  USE IN COMPUTATION*)

let disallowed_age_status : (int * age_status) ref =
  ref (-1, { over_21 = false; under_21 = false })

let compute_disallowed_ages day =
  let store_day, ages = !disallowed_age_status in
  if store_day = day then ages
  else (
    disallowed_age_status :=
      ( day,
        {
          over_21 = not (ParseRules.over_21_allowed day);
          under_21 = not (ParseRules.under_21_allowed day);
        } );
    snd !disallowed_age_status)

(*[change_fname doc person] changes the first name of a document and
  returns a new document of this type with a new name.*)

let change_fname (doc : Documents.doc_type) =
  let old_fname = Documents.get_fname doc in

  try
    ( Documents.remake_doc_fname doc
        (resample
           (fun () -> Person.make_fname (Documents.get_doc_gender doc))
           old_fname),
      "Inconsistent first name" )
  with _ ->
    ( Documents.remake_doc_fname doc
        (resample (fun () -> Person.make_random_fname ()) old_fname),
      "Inconsistent first name" )

(*[change_lname doc] is the same as [change_fname doc person], but for
  last names *)
let change_lname (doc : Documents.doc_type) =
  let old_lname = Documents.get_lname doc in

  ( Documents.remake_doc_lname doc (resample Person.make_lname old_lname),
    "Inconsistent last name" )

(*Could decouple code by using resample here*)

(*[misspell_fname doc] creates a first name for document [doc] that is
  misspelled*)
let misspell_fname (doc : Documents.doc_type) =
  let old_fname = Documents.get_fname doc in
  let letter_idx = Random.int (String.length old_fname) in
  let old_letter = old_fname.[letter_idx] in

  let rec create_misspell () =
    let new_letter = Char.chr (Random.int 26 + 97) in
    if new_letter <> old_letter then
      String.capitalize_ascii
        (replace_string_char old_fname letter_idx new_letter)
    else create_misspell ()
  in

  ( Documents.remake_doc_fname doc (create_misspell ()),
    "Misspelled first name" )

(*[misspell_lname doc] creates a last name for document [doc] that is
  misspelled*)
let misspell_lname (doc : Documents.doc_type) =
  let old_lname = Documents.get_lname doc in
  let letter_idx = Random.int (String.length old_lname) in
  let old_letter = old_lname.[letter_idx] in

  let rec create_misspell () =
    let new_letter = Char.chr (Random.int 26 + 97) in
    if new_letter <> old_letter then
      replace_string_char old_lname letter_idx new_letter
    else create_misspell ()
  in

  ( Documents.remake_doc_lname doc (create_misspell ()),
    "Misspelled last name" )

(*[misspell_netid doc] creates an incorrect netid for document [doc]*)

let misspell_netid (doc : Documents.doc_type) =
  let idx = Random.int 2 and netid = Documents.get_netid doc in
  let choice_letter =
    resample (fun () -> Char.chr (Random.int 26 + 97)) netid.[idx]
  in
  ( Documents.remake_doc_netid doc
      (replace_string_char netid idx choice_letter),
    "Inconsistent netid" )

(*[make_invalid_exp_date doc date] makes an invalid expiration date for
  [doc] (after current day [date])*)

(*Note: we will need to print the current day to use this*)

let make_invalid_exp_date
    (doc : Documents.doc_type)
    (curr_date : Date.date) =
  ( Documents.remake_doc_expiration doc
      (Date.reverse_multiple_days curr_date (Random.int 364 + 1)),
    "Invalid expiration date" )

(*[make_invalid_id_num doc] makes an invalid ID number for document
  [doc]*)

let make_invalid_id_num (doc : Documents.doc_type) =
  let old_id = Documents.get_id doc in

  let idx = Random.int (String.length old_id) in

  let choice_char =
    resample (fun () -> Char.chr (Random.int 10 + 48)) old_id.[idx]
  in

  ( Documents.remake_doc_id doc
      (replace_string_char old_id idx choice_char),
    "Inconsistent ID number" )

let make_invalid_order_vax_dates (doc : Documents.doc_type) =
  (Documents.swap_vax_dates doc, "Inconsistent vaccination dates")

(*Beginning of day-specific functions*)

(*[remove_doc doc] removes [doc] from this person's documents by
  providing an empty document.*)

let remove_doc (doc : Documents.doc_type) = Documents.make_none_doc ()

(*[disallow_role doc] turns the role of [doc] to a disallowed role if
  [doc] is a Cornell ID. *)

let disallow_role day (doc : Documents.doc_type) =
  let disallowed_role_list = compute_disallowed_roles day in

  let chosen_role =
    List.nth disallowed_role_list
      (Random.int (List.length disallowed_role_list))
  in
  (Documents.remake_doc_role doc chosen_role, "Unpermitted occupation")

(*[disallow_gender doc] creates a document like [doc] with a disallowed
  gender*)
let disallow_gender day (doc : Documents.doc_type) =
  let disallowed_gender_list = compute_disallowed_genders day in

  let chosen_gender =
    List.nth disallowed_gender_list
      (Random.int (List.length disallowed_gender_list))
  in
  (Documents.remake_doc_gender doc chosen_gender, "Unpermitted gender")

let only_boosted doc empty_date_input =
  if Random.bool () then
    ( Documents.remake_vax_dates doc empty_date_input empty_date_input
        empty_date_input,
      "Unvaccinated" )
  else
    ( Documents.remake_vax_dates doc
        (Documents.get_dose1_date doc)
        (Documents.get_dose2_date doc)
        empty_date_input,
      "2x Vaccinated" )

let only_vaccinated doc empty_date_input =
  ( Documents.remake_vax_dates doc empty_date_input empty_date_input
      empty_date_input,
    "Unvaccinated" )

let disallow_vaccine day (doc : Documents.doc_type) =
  let empty_date_input = Date.make_nonempty_date 0 0 0 in
  let disallowments = compute_disallowed_vaccinations day in
  if
    disallowments.unvax
    && (not disallowments.double)
    && not disallowments.boosted
  then only_vaccinated doc empty_date_input
  else if
    disallowments.unvax && disallowments.double
    && not disallowments.boosted
  then only_boosted doc empty_date_input
  else failwith "Uncovered vaccination case"

let make_multiple_dob_doc doc1 doc2 dob str =
  ( Documents.remake_doc_dob doc1 dob,
    Documents.remake_doc_dob doc2 dob,
    str )

let disallow_age
    day
    date
    (doc1 : Documents.doc_type)
    (doc2 : Documents.doc_type) =
  let config = compute_disallowed_ages day in
  if config.under_21 then
    let under_21_dob = Date.make_under_21 date in
    make_multiple_dob_doc doc1 doc2 under_21_dob "Under 21"
  else if config.over_21 then
    let over_21_dob = Date.make_over_21 date in
    make_multiple_dob_doc doc1 doc2 over_21_dob "Over 21"
  else failwith "Uncovered age case"

(*End of day-specific functions*)

(** TLDR: my idea for this is to literally select a random function from
    an array + run it to invalidate the vaccine card. We can also run
    multiple.*)

let invalid_vaccine_card_functions =
  [
    change_fname;
    change_lname;
    misspell_fname;
    misspell_lname;
    make_invalid_order_vax_dates;
  ]

let invalid_school_id_functions =
  [
    change_fname;
    change_lname;
    misspell_fname;
    misspell_lname;
    misspell_netid;
    make_invalid_id_num;
  ]

let invalid_drivers_license_functions date =
  [
    change_fname;
    change_lname;
    misspell_fname;
    misspell_lname;
    make_invalid_id_num;
    (fun x -> make_invalid_exp_date x date);
  ]

(*Output of these is a document and a reason for failure. We will only
  be having one error in each incorrect document to make life easy in
  coding. We can make this more complicated once we know how it affects
  the game*)

let invalidate_document doc func_list =
  let func_choice =
    List.nth func_list (Random.int (List.length func_list))
  in

  func_choice doc

let make_invalid_vaccine_card (card : Documents.doc_type) =
  invalidate_document card invalid_vaccine_card_functions

let make_invalid_school_id (id : Documents.doc_type) =
  invalidate_document id invalid_school_id_functions

let make_invalid_drivers_license
    (license : Documents.doc_type)
    (date : Date.date) =
  invalidate_document license (invalid_drivers_license_functions date)

(*[basic_invalidation_output valid_docs date] invalidates documents
  based on basic flaws, not daily rules*)
let basic_invalidation_output valid_docs date =
  let choose_failure_doc = Random.int 3 in
  if choose_failure_doc = 0 then
    let doc = Documents.get_vaccine_card valid_docs in
    let vax, str = make_invalid_vaccine_card doc in
    (Documents.reset_docs_vaccine_card valid_docs vax, str)
  else if choose_failure_doc = 1 then
    let doc = Documents.get_id_card valid_docs in
    let school_id, str = make_invalid_school_id doc in
    (Documents.reset_docs_school_id valid_docs school_id, str)
  else
    let doc = Documents.get_license valid_docs in
    let license, str = make_invalid_drivers_license doc date in
    (Documents.reset_docs_license valid_docs license, str)

(*[check_gender day] is true if there exist gender rules s.t. they can
  be invalidated in a document. *)
let check_gender day = List.length (compute_disallowed_genders day) > 0
let check_role day = List.length (compute_disallowed_roles day) > 0

let check_vaccine day =
  let config = compute_disallowed_vaccinations day in
  config.unvax || config.double || config.boosted

let check_age day =
  let config = compute_disallowed_ages day in
  config.under_21 || config.over_21

(*[check_all] is true if there exists a rule to enforce *)
let check_all day =
  check_gender day || check_age day || check_role day
  || check_vaccine day

(*[construct_legal constructs a list of legal operations]*)
let construct_legal day =
  let append_lst = ref [] in
  if check_age day then append_lst := "age" :: !append_lst;
  if check_gender day then append_lst := "gender" :: !append_lst;
  if check_role day then append_lst := "role" :: !append_lst;
  if check_vaccine day then append_lst := "vax" :: !append_lst;
  !append_lst

(*Actually invalidates documents*)

let manage_reset_age docs day date vaccine_card license =
  let vax, license, str = disallow_age day date vaccine_card license in
  ( Documents.reset_docs_license
      (Documents.reset_docs_vaccine_card docs vax)
      license,
    str )

let manage_reset_gender docs day license =
  let new_license, str = disallow_gender day license in
  (Documents.reset_docs_license docs new_license, str)

let manage_reset_vax docs day vaccine_card =
  let vax, str = disallow_vaccine day vaccine_card in
  (Documents.reset_docs_vaccine_card docs vax, str)

let manage_reset_role docs day school_id =
  let id, str = disallow_role day school_id in
  (Documents.reset_docs_school_id docs id, str)

let invalidate_docs_by_rule docs day date =
  let legal_rules = construct_legal day in
  let license = Documents.get_license docs
  and vaccine_card = Documents.get_vaccine_card docs
  and school_id = Documents.get_id_card docs in
  let choice_rule =
    List.nth legal_rules (Random.int (List.length legal_rules))
  in
  if choice_rule = "age" then
    manage_reset_age docs day date vaccine_card license
  else if choice_rule = "gender" then
    manage_reset_gender docs day license
  else if choice_rule = "vax" then
    manage_reset_vax docs day vaccine_card
  else if choice_rule = "role" then manage_reset_role docs day school_id
  else failwith "Uncovered document modification"

(*Outputs a document and a failure reason*)
let make_final_docs person date day_idx =
  let valid_docs = Documents.make_valid_documents day_idx date person in

  if Person.check_valid_docs person then
    (valid_docs, "Valid documentation")
  else if not (check_all day_idx) then
    basic_invalidation_output valid_docs date
  else if Random.bool () then
    invalidate_docs_by_rule valid_docs day_idx date
  else basic_invalidation_output valid_docs date
(*find a rule to invalidate, if one doesn't exist, then repeat basic
  invalidation. Run things based on length! *)
