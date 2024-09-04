open Util

let read_from_file f =
  let ic = open_in f and file_lines = ref [] in
  try
    while true do
      file_lines := input_line ic :: !file_lines
    done;
    !file_lines
  with End_of_file ->
    close_in ic;
    List.rev !file_lines

let prompt_user str =
  print_string ("\n\n" ^ str);
  print_string "> ";
  read_line ()

let print_file (f : string) = List.iter print_endline (read_from_file f)
let print_list (lst : string list) = List.iter print_endline lst

(* DO NOT move printing functions for people/documents/state/etc into
   this module. It will cause a cycle.

   Instead: implement printing functions within Person, Documents, etc.
   and utilize the functions in this module as helpers*)

(* TODO THIS IS TEMPORARY UNTIL THERE IS A NEW MODULE THAT'S CREATED*)
let rec string_to_char_list s =
  match s with
  | "" -> []
  | h ->
      if String.length h = 1 then [ h.[0] ]
      else
        h.[0]
        :: string_to_char_list (String.sub h 1 (String.length h - 1))

let erase_delay_char l delay =
  for x = 1 to l do
    ANSITerminal.move_cursor (-1) 0;
    Unix.sleepf delay;
    ANSITerminal.erase Eol
  done

let next_line_helper (next_line : bool) =
  if next_line then
    let _ =
      ANSITerminal.scroll 1;
      ANSITerminal.move_bol ()
    in
    ANSITerminal.move_cursor 0 1
  else ANSITerminal.move_cursor 0 0

let print_delay_char (s : string) (delay : float) (next_line : bool) =
  let lst = ref (explode s) in

  for x = 1 to List.length !lst do
    if x > 1 then ANSITerminal.move_cursor 1 1
    else ANSITerminal.move_cursor 0 0;

    Unix.sleepf delay;
    print_char (List.hd !lst);
    ANSITerminal.move_cursor (-1) 0;
    lst := List.tl !lst
  done;
  next_line_helper next_line

let rec print_delay_string (l : string list) (delay : float) =
  match l with
  | [] -> print_string ""
  | h :: t ->
      Unix.sleepf delay;
      print_string h
