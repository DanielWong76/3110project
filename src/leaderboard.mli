type score
(** The type that stores one individual score *)

type leaderboard
(** The type that stores the scores of a minesweeper game instance sorted in
    descending order *)

val empty : unit -> leaderboard
(** Creates an empty leaderboard *)

val create_score : string -> int -> float -> int -> score
(** [create_score name score time tiles] creates a score with the given
    attributes *)

val add_score : score -> leaderboard -> leaderboard
(** [add_score s l] adds [s] to [l] *)

val return_top_n : int -> leaderboard -> string
(** [return_top_n n l] returns a string representation of the top
    [min n l.length] scores in [l]. *)

val get_score_of : string -> leaderboard -> string
(** [get_score_of n l] returns the string rep of the highest score of [n] in
    [l]. Raises: Not_found if [n] does not exist in [l] *)

val size : leaderboard -> int

val export_leaderboard : leaderboard -> string
(** [export_leaderboard l] returns a string representation of [l] to be printed
    out in a file in the form: Name Score Time_Taken Tiles_Opened *)

val import_leaderboard : in_channel -> leaderboard
