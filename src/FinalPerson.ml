type final_person = Person.person * Documents.documents * string

let get_person final_person =
  match final_person with
  | person, _, _ -> person

let get_documents final_person =
  match final_person with
  | _, docs, _ -> docs

let get_reason_string final_person =
  match final_person with
  | _, _, t -> "Penalty: " ^ t

(* The date passed into make_final_person should be the current game day
   from State. It ensures that the expiration dates on the documents are
   correct.*)
let make_final_person state =
  let date = State.get_current_day state in
  let day_idx = State.get_index_day state in
  let person = Person.make_person day_idx date in

  let docs = InvalidDoc.make_final_docs person date day_idx in
  (person, fst docs, snd docs)
