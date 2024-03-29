type score = {
  name : string;
  score : int;
  time_taken : float;
  tiles_opened : int;
}

type leaderboard = score list

let empty () : leaderboard = []

let create_score name score time tiles =
  { name; score; time_taken = time; tiles_opened = tiles }

let add_score score (leaderboard : leaderboard) : leaderboard =
  let rec add_score_rec score leaderboard =
    match leaderboard with
    | [] -> score :: leaderboard
    | h :: t ->
        if h.score >= score.score then h :: add_score_rec score t
        else score :: h :: t
  in
  add_score_rec score leaderboard

let score_to_string score =
  score.name ^ ", Score: " ^ string_of_int score.score ^ ", Time Taken: "
  ^ string_of_float score.time_taken
  ^ " s, Tiles Opened: "
  ^ string_of_int score.tiles_opened

let return_top_n number leaderboard =
  let actual_returns = min (List.length leaderboard) number in
  let rec return_top_n_rec curr target (leaderboard : leaderboard) =
    if curr > target then ""
    else
      match leaderboard with
      | [] -> ""
      | h :: t ->
          let curr_score = h in
          "\n"
          ^ string_of_int (curr + 1)
          ^ ". " ^ score_to_string curr_score
          ^ return_top_n_rec (curr + 1) number t
  in
  return_top_n_rec 0 actual_returns leaderboard ^ "\n"

let rec get_score_of name (leaderboard : leaderboard) =
  match leaderboard with
  | [] -> raise Not_found
  | h :: t -> if h.name = name then score_to_string h else get_score_of name t

let size leaderboard = List.length leaderboard

let export_leaderboard leaderboard =
  let rec score_string l count =
    match l with
    | [] -> ""
    | h :: t ->
        string_of_int count ^ ". " ^ h.name ^ " " ^ string_of_int h.score ^ " "
        ^ string_of_float h.time_taken
        ^ " "
        ^ string_of_int h.tiles_opened
        ^ "\n"
        ^ score_string t (count + 1)
  in
  "Leaderboard\n"
  ^ string_of_int (List.length leaderboard)
  ^ "\n" ^ score_string leaderboard 1 ^ "Leaderboard End\n"

let rec parse_scores ic leaderboard num =
  if num <= 0 then leaderboard
  else
    try
      let line = input_line ic in
      let raw = String.split_on_char ' ' line in
      let sname = List.nth raw 1 in
      let sscore = int_of_string (List.nth raw 2) in
      let stime_taken = float_of_string (List.nth raw 3) in
      let stiles_opened = int_of_string (List.nth raw 4) in
      let new_score =
        {
          name = sname;
          score = sscore;
          time_taken = stime_taken;
          tiles_opened = stiles_opened;
        }
      in
      let new_leaderboard = add_score new_score leaderboard in
      parse_scores ic new_leaderboard (num - 1)
    with e ->
      close_in_noerr ic;
      raise e

let import_leaderboard ic =
  let new_leaderboard = empty () in
  try
    let num = int_of_string (input_line ic) in
    let res = parse_scores ic new_leaderboard num in
    if input_line ic = "Leaderboard End" then res
    else failwith "Missing Leaderboard End"
  with e ->
    close_in_noerr ic;
    raise e
