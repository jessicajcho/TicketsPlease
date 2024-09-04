(** This module represents the person and the various functionality
    involved with that. *)

type gender
(**[gender] is a type representing a person's gender. Examples include
   male, female, and other. *)

type member
(**[member] is a type representing what kind of community member someone
   is. Examples include faculty, students, and other. *)

type person
(** [person] is a type representing a person. It has many attributes
    accessible with the functions below. *)

val get_fname : person -> string
(** [get_fname p] retrieves the first name for person [person].*)

val get_lname : person -> string
(** [get_lname p] retrieves the last name for person [person].*)

val get_dob : person -> Date.date
(** [get_dob p] retrieves the birth date for person [person].*)

val get_id : person -> string
(** [get_id p] retrieves the id for person [person].*)

val get_gender : person -> gender
(** [get_gender p] retrieves the gender for person [person].*)

val get_role : person -> member
(** [get_role p] retrieves the community role for person [person].*)

val get_family : person -> person list
(** [get_family p] retrieves the family of person [person].*)

val get_hair_color : person -> string
(**[get_hair_color p] is the hair color of person [p]*)

val check_valid_docs : person -> bool
(** [check_valid_docs p] is [true] if [person] has all valid documents,
    and [false] otherwise.*)

val gender_to_string : gender -> string

val get_height : person -> int
(** [get_height p] retrieves the height for person [person].*)

val get_eye_color : person -> string
(** [get_eye_color p] retrieves the eye color of person [person].*)

val get_hair_color : person -> string
(** [get_hair_color p] retrieves the hair color of person [person].*)

val make_lname : unit -> string
(** [make_lname ()] makes a random last name.*)

val make_fname : gender -> string
(** [make_fname g] makes a random first name for someone of gender
    [gender].*)

val make_person : int -> Date.date -> person
(** [make_person day] makes a random person with all of his or her
    traits accessible with module functions on [day]. [day] is important
    to get the attributes of a valid person correct for a given day.*)

val make_gender : int -> gender
(**[make_gender day] makes a random gender based on the rules of [day]*)

val gender_to_string : gender -> string
(** [gender_to_string g] returns a string representing the gender
    [gender].*)

val role_to_string : member -> string
(** [role_to_string g] returns a string representing the role of member
    [member].*)

val set_gender : string -> gender
(**[set_gender g] returns the gender type associated with string [g].
   REQUIRES: [g] is some captialization variant of "m", "male", "f",
   "female", "o", "other"*)

val set_role : string -> member
(**[set_role m] returns the member type represented by the string [m].
   Accepted strings include any capitalization of "other", "faculty",
   and "student". Raises an error if an unrecognizable string is
   entered.*)

val make_random_fname : unit -> string

(**[make_random_fname ()] is a random first name of a random gender.*)
