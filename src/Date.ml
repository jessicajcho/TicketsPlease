type date =
  | NoDate
  | Date of {
      day : int;
      month : int;
      year : int;
    }

let get_day date =
  match date with
  | NoDate -> failwith "No date, can't get day"
  | Date t -> t.day

let get_month date =
  match date with
  | NoDate -> failwith "No date, can't get month"
  | Date t -> t.month

let get_year date =
  match date with
  | NoDate -> failwith "No date, can't get year"
  | Date t -> t.year

let make_nonempty_date day month year = Date { day; month; year }
let make_empty_date () = NoDate

let date_exists date =
  match date with
  | NoDate -> false
  | Date _ -> true

let get_day_string date = string_of_int (get_day date)
let get_month_string date = string_of_int (get_month date)
let get_year_string date = string_of_int (get_year date)

let date_to_string date =
  let day = get_day_string date in
  let len = String.length day in
  let string_day =
    if len > 2 then failwith "Day is longer than 2 digits."
    else if len = 2 then day
    else "0" ^ day
  in
  get_month_string date ^ "/" ^ string_day ^ "/" ^ get_year_string date

let rec advance_multiple_days_helper date day month_idx yf month_lst =
  if day > List.nth month_lst month_idx then
    if month_idx = 11 then
      advance_multiple_days_helper date
        (day - List.nth month_lst month_idx)
        0 (yf + 1) month_lst
    else
      advance_multiple_days_helper date
        (day - List.nth month_lst month_idx)
        (month_idx + 1) yf month_lst
  else make_nonempty_date day (month_idx + 1) (get_year date + yf)

let month_lst = [ 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 ]

let advance_multiple_days (date : date) (days_to_adv : int) : date =
  let start_month_idx = get_month date - 1 in
  let start_day = get_day date in
  match date with
  | NoDate -> failwith "None date, no days to advance"
  | Date t ->
      advance_multiple_days_helper date
        (days_to_adv + start_day)
        start_month_idx 0 month_lst

let manage_reverse_date date i curr_day final_month_idx years_backward =
  while !i > 0 do
    i := !i - 1;
    if !curr_day > 1 then curr_day := !curr_day - 1
    else if !curr_day = 1 && !final_month_idx = 0 then begin
      curr_day := 31;
      final_month_idx := 11;
      years_backward := !years_backward + 1
    end
    else begin
      final_month_idx := !final_month_idx - 1;
      curr_day := List.nth month_lst !final_month_idx
    end
  done;
  make_nonempty_date !curr_day (!final_month_idx + 1)
    (get_year date - !years_backward)

let reverse_multiple_days (date : date) (days_to_rev : int) : date =
  let start_month_idx = get_month date - 1 in
  let start_day = get_day date in
  match date with
  | NoDate -> failwith "None date, no days to advance"
  | Date t ->
      let i = ref days_to_rev
      and curr_day = ref start_day
      and final_month_idx = ref start_month_idx
      and years_backward = ref 0 in
      manage_reverse_date date i curr_day final_month_idx years_backward

let reverse_multiple_years (date : date) (years_to_rev : int) : date =
  match date with
  | NoDate -> failwith "No date to reverse"
  | Date d -> Date { d with year = d.year - years_to_rev }

(*let advance_multiple_years (date : date) (years_to_adv : int) : date =
  match date with | NoDate -> failwith "No date to advance" | Date d ->
  Date { d with year = d.year + years_to_adv }*)

(*Might want to export M/D/Y *)
let next_date date = advance_multiple_days date 1

let make_over_21 date =
  reverse_multiple_days
    (reverse_multiple_years date (Random.int 60 + 21))
    (Random.int 229)

let make_under_21 date =
  advance_multiple_days
    (reverse_multiple_years date (Random.int 10 + 11))
    (Random.int 200 + 1)
