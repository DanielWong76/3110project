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
  let rec score_string l =
    match l with
    | [] -> ""
    | h :: t ->
        h.name ^ " " ^ string_of_int h.score ^ " "
        ^ string_of_float h.time_taken
        ^ " "
        ^ string_of_int h.tiles_opened
        ^ "\n" ^ score_string t
  in
  score_string leaderboard
