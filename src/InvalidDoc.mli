(** This module handles creating invalid docs for a person. *)

val make_final_docs :
  Person.person -> Date.date -> int -> Documents.documents * string
(**[make_final_docs person curr_date] makes the final (correct or
   incorrect) documents to display to the player for a [person] and
   current date [curr_date], as well as a string reason for why a player
   has given the wrong result (based on if they accepted or rejected).*)
