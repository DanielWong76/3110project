open Minesweeper
open Grid

(** Execution of the game printing and playing in terminal *)

val leaderboard : Leaderboard.leaderboard ref
(** Mutable leaderboard *)

val export_game : grid -> Leaderboard.leaderboard -> string -> unit
(** [export_game g l f]Takes in a grid, g, and leaderboard, l, instance and
    writes it to [f] location. *)

val import_game : string -> grid
(** [import_game file_name] mutates [leaderboard] to be the leaderboard
    contained in [file_name] and returns a grid instance saved in [file_name] *)

val initialize : string -> grid
(** [initialize s] initializes grid matching s, where s should be "easy",
    "medium", or "hard". If s is none of the above, it initializes a 0x0 grid
    with 0 bombs*)

val choose : unit -> string
(** Prompts user to choose a difficulty and returns the corresponding chosen
    difficulty as a string *)

val open_tile : int * int -> grid -> grid
(** [open_tile (x,y) gr] opens the tile of gr located at (x,y). If tile is
    Out_of_Bounds, or Already_Revealed, will re-prompt users to pick a new tile.
    If tile is a bomb, will start the Game Over sequence *)

val flagged_tile : int * int -> grid -> grid
(** [flagged_tile (x,y) gr] flags the tile in gr at (x,y). If tile is
    Out_of_Bounds, or Already_Revealed, will re-prompt users to pick a new tile. *)

val choose_action : unit -> string
(** Prompts user to choose an action and returns the corresponding chosen action
    as a string*)

val get_string : unit -> string
(** Prompts user to input their name for the leaderboard and returns it as a
    string. *)

val calculate_score : grid -> bool -> int
(** [calculate_score gr b] returns the score of gr, where b is true if the
    player won and false if the player lost *)

val on_game_end : grid -> bool -> unit
(** End game sequence for leaderboard. Prints the player's score and asks if
    they'd like to save it. If yes, prompts user for their name. Prints the
    leaderboard*)

val on_death : unit -> unit
(** End game sequence prompting user to try again or ending the game. *)

val pictionary : unit -> unit
(** Controls pictionary mode. *)

val main : unit -> unit
(** Executes the game. *)
