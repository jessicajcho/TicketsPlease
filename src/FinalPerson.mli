(** This module represents a final person after changing its properties
    and the functionality surrouding that. *)

type final_person
(**[final_person] represents the final person *)

val get_person : final_person -> Person.person
(**[get_person final_person] returns the person instance of this unit*)

val get_documents : final_person -> Documents.documents
(**[get_documents final_person] returns the documents associated wit
   this [final_person]*)

val make_final_person : State.t -> final_person
(**[make_final_person st] makes a [final_person] with a person instance,
   documents instance, and reason string using state [st]*)

val get_reason_string : final_person -> string
(**[get_reason_string final_person] returns the reason for user failure
   (in the event of a bad decision) for this [final_person]*)
