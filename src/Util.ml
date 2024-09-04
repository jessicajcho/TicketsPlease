open Yojson.Basic.Util

let explode s =
  let rec exp_each i lst =
    if i < 0 then lst else s.[i] :: exp_each (i - 1) lst
  in

  List.rev (exp_each (String.length s - 1) [])

let implode l =
  List.fold_left (fun x y -> String.make 1 y ^ x) "" (List.rev l)

let replace_string_char s i chr =
  let rec replace lst (acc : int) =
    match lst with
    | [] -> []
    | h :: t -> if acc = i then chr :: t else h :: replace t (acc + 1)
  in

  implode (replace (explode s) 0)

let rand_elem lst =
  let ind = Random.int @@ List.length lst in
  List.nth lst ind

let printing_json =
  to_assoc
    (Yojson.Basic.from_file
       ("data" ^ Filename.dir_sep ^ "json" ^ Filename.dir_sep
      ^ "printing.json"))

let retrieve_string str = to_string (List.assoc str printing_json)
