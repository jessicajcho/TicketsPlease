type object_phrase = string list

type command =
  | Play
  | Accept of object_phrase
  | Reject of object_phrase
  | Buy of object_phrase
  | Continue
  | Help
  | Quit
  | Rules
  | Inventory
  | Money

exception Empty
exception Malformed

let split_func str =
  String.split_on_char ' ' str |> List.filter (fun x -> x <> "")

(*I'm not sure we really need strict guidelines on what comes after
  "quit" or "play"*)
let parse str =
  let init_split = split_func str in
  match init_split with
  | [] -> raise Empty
  | [ "play" ] -> Play
  | [ "help" ] -> Help
  | [ "quit" ] -> Quit
  | [ "Quit" ] -> Quit
  | [ "Rules" ] | [ "rules" ] -> Rules
  | [ "continue" ] -> Continue
  | [ "inventory" ] | [ "Inventory" ] -> Inventory
  | [ "money" ] | [ "Money" ] -> Money
  | "accept" :: t -> Accept t
  | "reject" :: t -> Reject t
  | "buy" :: t -> Buy t
  | h :: t -> raise Malformed
