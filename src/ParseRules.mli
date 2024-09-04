(** This module handles the parsing and setup of the game from the JSON
    file. *)

val get_day_begin_story : int -> string
(**[get_day_begin_story day] is the string of beginning storyline on
   that [day].*)

val get_day_end_story : int -> string
(**[get_day_end_story day] is the string of ending storyling for that
   [day]*)

val get_day_rules : int -> string list
(**[get_day_rules day] is the string list of rules for that day to be
   printed to the user upon entering the "rules" command. *)

val num_days : int
(**[num_days] is the number of days of defined gameplay. *)

val students_allowed : int -> bool
(**[students_allowed day] determines if students are allowed in on [day]*)

val faculty_allowed : int -> bool
(**[faculty_allowed day] determines if faculty are allowed in on [day]*)

val other_role_allowed : int -> bool
(**[other_role_allowed day] determines if non-Cornell individuals are
   allowed in on [day]*)

val under_21_allowed : int -> bool
(**[under_21_allowed day] determines if individuals under 21 are allowed
   in on [day]*)

val over_21_allowed : int -> bool
(**[over_21_allowed day] determines if individuals over 21 are allowed
   in on [day]*)

val unvax_allowed : int -> bool
(**[unvax_allowed day] determines if unvaccinated individuals are
   allowed in on [day]*)

val vax_2x_allowed : int -> bool
(**[vax_2x_allowed day] determines if doubly vaccinated individuals are
   allowed in on [day]*)

val vax_3x_allowed : int -> bool
(**[vax_3x_allowed day] determines if boosted individuals are allowed in
   on [day]*)

val female_allowed : int -> bool
(**[female_allowed day] determines if women are allowed in on [day]*)

val other_gender_allowed : int -> bool
(**[other_gender_allowed day] determines if gender non-conforming
   individuals are allowed in on [day]*)

val male_allowed : int -> bool
(**[male_allowed day] determines if men are allowed in on [day]*)
