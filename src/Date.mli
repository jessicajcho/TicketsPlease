(** This module represents the date and the functionality surrounding
    that. *)

type date
(** [date] is a type representing a calendar date, with a day, month,
    and year.*)

val make_nonempty_date : int -> int -> int -> date
(** [make_nonempty_date d m y] makes a non-empty date given a day [d],
    month [m], and year [y]. *)

val get_day : date -> int
(** [get_date d] retrieves the day of date [d]*)

val get_month : date -> int
(** [get_month d] retrieves the month of date [d]*)

val get_year : date -> int
(** [get_year d] retrieves the year of date [d]*)

val make_empty_date : unit -> date
(** [make_empty_date ()] makes an empty date type representing the
    absence of a date.*)

val get_year_string : date -> string
(**[get_year_string d] retrieves the string representation of the year
   in date [d]*)

val get_day_string : date -> string
(**[get_day_string d] retrieves the string representation of the day in
   date [d]*)

val get_month_string : date -> string
(**[get_month_string d] retrieves the string representation of the month
   in date [d]*)

val date_to_string : date -> string
(**[date_to_string d] formats date [d] as a string in the pattern
   [dd/mm/yyyy]*)

val date_exists : date -> bool
(**[date_exists d] is [true] if [d] is not the empty date. Otherwise,
   this expression is [false]*)

val next_date : date -> date
(** [next_date d] increments the date [d] to the next day.*)

val advance_multiple_days : date -> int -> date
(** [advance_multiple_date date i] is the date that is [i] days after
    date [date]. Requires: [i] is a nonnegative integer*)

val reverse_multiple_days : date -> int -> date
(** [reverse_multiple_date date i] is the date that is [i] days before
    date [date]. Requires: [i] is a nonnegative integer*)

val reverse_multiple_years : date -> int -> date
(** [reverse_multiple_years date y] is the date that is [y] years before
    [date]. Requires: The new date must be in a year after 0 AD .*)

val make_over_21 : date -> date
(**[make_over_21 date] is a date that is at least 21 years before [date]*)

val make_under_21 : date -> date
(**[make_over_21 date] is a date that is within 21 years of [date]*)
