(** This module represents the different types of documents and the
    functionality surrounding those documents. *)

type documents
(** [documents] is a type representing a collection of documents a
    person can have.*)

type print_item

(**[print_item] is a type representing a printing format for a document*)

type covid_vaccination_card
(** [covid_vaccination_card] is a type representing a COVID vaccine
    card.*)

type school_id
(** [school_id] is a type representing a Cornell ID.*)

type drivers_license
(** [drivers_license] is a type representing a driver's license.*)

type doc_type
(**[doc_type] is a type that encapsulates a document *)

val make_valid_documents :
  int -> Date.date -> Person.person -> documents
(**[make_valid_documents day_idx date p ] is a set of valid documents
   make on date [date] for person [p] on index day [day_idx]*)

val get_vaccine_card : documents -> doc_type
(**[get_vaccine_card docs] retrieves the vaccine card in a set of
   documents [docs]*)

val get_netid : doc_type -> string
(**[get_netid doc] gets the corresponding netid from document [doc]*)

val get_id_card : documents -> doc_type
(**[get_id_card docs] retrieves the school id card in a set of documents
   [docs]*)

val get_license : documents -> doc_type
(**[get_license docs] retrieves the driver's license in a set of
   documents [docs]*)

val get_fname : doc_type -> string
(**[get_fname doc] returns the first name on a document [doc].contents
   Raises: [Failure] if the given document does not have a first name *)

val get_lname : doc_type -> string
(**[get_lname doc] returns the last name on a document [doc].contents
   Raises: [Failure] if the given document does not have a last name *)

val get_eye_color : doc_type -> string
(**[get_eye_color doc] returns the eye color on a document
   [doc].contents Raises: [Failure] if the given document does not have
   an eye color listed*)

val get_height : doc_type -> int
(**[get_height doc] returns the height on a document [doc].contents
   Raises: [Failure] if the given document does not have a height *)

val get_id : doc_type -> string
(**[get_id doc] returns the id on a document [doc].contents Raises:
   [Failure] if the given document does not have an id *)

val get_expiration : doc_type -> string
(**[get_expiration doc] returns the expiration date (string) on a
   document [doc].contents Raises: [Failure] if the given document does
   not have an expiration *)

val get_expiration_date : doc_type -> Date.date
(**[get_expiration_date doc] returns the expiration date (Date.date) on
   a document [doc].contents Raises: [Failure] if the given document
   does not have an expiration *)

val get_dose_1 : doc_type -> string
(**[get_dose_1 doc] returns the first dose date (string) on a document
   [doc].contents Raises: [Failure] if the given document does not have
   a first dose date*)

val get_dose_2 : doc_type -> string
(**[get_dose_2 doc] returns the second dose date (string) on a document
   [doc].contents Raises: [Failure] if the given document does not have
   a second dose date*)

val get_booster : doc_type -> string
(**[get_booster doc] returns the booster date (string) on a document
   [doc].contents Raises: [Failure] if the given document does not have
   a booster date*)

val remake_doc_fname : doc_type -> string -> doc_type
(**[remake_doc_fname doc fname] takes document [doc] and resets the
   first name to [fname], keeping everything else the same (including
   document type). This raises a failure if [doc] does not have a first
   name*)

val remake_doc_lname : doc_type -> string -> doc_type
(**[remake_doc_lname doc lname] takes document [doc] and resets the last
   name to [fname], keeping everything else the same (including document
   type). This raises a failure if [doc]does not have a last name. *)

val remake_doc_id : doc_type -> string -> doc_type
(**[remake_doc_id doc id] takes document [doc] and resets the id to
   [id]. Raises a failure if [doc] does not have an id. *)

val remake_doc_netid : doc_type -> string -> doc_type
(**[remake_doc_netid doc netid] takes document [doc] and resets the
   netid to [netid]. Raises a failure if [doc] does not have a netid*)

val remake_doc_expiration : doc_type -> Date.date -> doc_type
(**[remake_doc_expiration doc exp] takes document [doc] and resets the
   expiration date to [exo]. Raises a failuree if [doc] does not have an
   expiration date *)

val remake_doc_gender : doc_type -> Person.gender -> doc_type
(** [remake_doc_gender doc g] takes document [doc] and resets its gender
    to [g]. Raises a failure if [doc] does not include a gender.*)

val remake_doc_role : doc_type -> Person.member -> doc_type
(** [remake_doc_role doc r] takes document [doc] and resets its role to
    [r]. Raises a failure if [doc] does not include a role.*)

val remake_doc_dob : doc_type -> Date.date -> doc_type
(** [remake_doc_dob doc dob] takes document [doc] and replaces its DOB
    with [dob]. Raises an error if [doc] does not have a DOB. *)

val swap_vax_dates : doc_type -> doc_type
(** [swap_vax_dates doc] randomly swaps two vaccination dates in [doc].
    Raises an error if [doc] does not have vaccination dates.*)

val remake_vax_dates :
  doc_type -> Date.date -> Date.date -> Date.date -> doc_type
(**[remake_vax_dates doc dose1 dose2 booster] is [doc] recreated with
   new dose dates [dose1], [dose2], and [booster]*)

val make_none_doc : unit -> doc_type
(**[make_none_doc ()] makes a none-type document (the equivalent of an
   empty/missing document)*)

val reset_docs_vaccine_card : documents -> doc_type -> documents
(**[reset_docs_vaccine_card docs vax] makes a new set of documents from
   an old set [docs] by keeping all other documents the same, but
   replacing the vaccine card with [vax]*)

val reset_docs_school_id : documents -> doc_type -> documents
(**[reset_docs_school_id docs id] makes a new set of documents from an
   old set [docs] by keeping all other documents the same, but replacing
   the school id with [id]*)

val get_dose1_date : doc_type -> Date.date
(**[get_dose1_date doc] is the dose 1 date of [doc]. Throws an error if
   [doc] is not a vaccine card.*)

val get_dose2_date : doc_type -> Date.date
(**[get_dose2_date doc] is the dose 2 date of [doc]. Throws an error if
   [doc] is not a vaccine card.*)

val reset_docs_license : documents -> doc_type -> documents
(**[reset_docs_vaccine_card docs license] makes a new set of documents
   from an old set [docs] by keeping all other documents the same, but
   replacing the driver's licence with [license]*)

val get_doc_gender : doc_type -> Person.gender
(**[get_doc_gender doc] is the gender on document [doc]. Raises an error
   if [doc] does not have a gender *)

val print_vaccine_card : doc_type -> unit
(**[print_vaccine_card] prints the vaccine card. *)

val print_school_id : doc_type -> unit
(**[print_school_id] prints the school id. *)

val print_drivers_license : doc_type -> unit
(**[print_drivers_license] prints the drivers liscense. *)

val print_documents : documents -> unit
(**[print_documents] prints all the documents. *)
