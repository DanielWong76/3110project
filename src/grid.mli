(** Representation and control of the minesweeper grid *)

type grid
(** The type [grid] represents the minesweeper grid *)

val empty : grid
(** Representation of empty grid*)

exception Out_of_Bounds
(** Raised when coords given are out of bounds for the given grid *)

exception Already_Revealed
(** Raised when [reveal_tile] attempts to reveal an already opened tile *)

exception Game_Over
(** Raised when [reveal_tile] opens a mine *)

val new_grid : int -> int -> int -> grid
(** [new_grid a b c] returns a new random grid of dimensions axb with c mines *)

val reveal_tile : int * int -> grid -> grid
(** [reveal_tile (a,b) c] opens the tile at location (a,b) in grid c and
    processes the corresponding action (ex. hitting a mine).

    Raises: [Out_of_Bounds] if (a,b) is out of bounds

    Raises: [Already_Revealed] if (a,b) is already open

    Raises: [Game_Over] if (a,b) is a mine*)

val flag_tile : int * int -> grid -> grid
(** [flag_tile (a,b) c] flags the tile at location (a,b) in grid c if tile is
    not already flagged. If tile is already flags, unflags tile.

    Raises: [Already_Revealed] if (a,b) is already open*)

val print_coord : grid -> int -> int -> bool -> string

val display_grid : grid -> unit
(** [display_grid a] prints a to the command line interface *)

val get_dimensions : grid -> int * int
(** [get_dimensions a] returns the dimensions of a *)

val get_dimensions_x : grid -> int
(** [get_dimensions_x a] returns the x of the dimension of a *)

val get_dimensions_y : grid -> int
(** [get_dimensions_y a] returns the y of the dimension of a *)

val get_mines : grid -> int
(** [get_mines a] returns the number of mines of a *)

val get_flagged : grid -> int
(** [get_flagged a] returns the number of flagged tiles of a *)

val reveal_all_mines : grid -> unit
(** [reveal_all_mines g] prints g with all player progress and mines revealed *)

val get_opened_tiles : grid -> int
(** [get_opened_tiles g] returns the number of opened tiles in [g] *)

val add_time : grid -> float -> grid
(** [add_time g t] returns a grid with the time of [t] to [g.time_taken] *)

val get_time_taken : grid -> float

val update_time : grid -> grid
(** [update_time g] returns a grid with time updated from now. *)

val export_grid : grid -> string
(** [export_grid g] returns a string representation of the information of [g] to
    be printed out in a file*)
