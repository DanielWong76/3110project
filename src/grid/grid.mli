(** Representation and control of the minesweeper grid *)

type grid
(** The type [grid] represents the minesweeper grid *)

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

    Raises: [Out_of_Bounds] if (a,b) is not contained in c

    Raises: [Already_Revealed] if (a,b) is already open *)

val display_grid : grid -> unit
(** [display_grid a] prints a to the command line interface *)

val get_dimensions : grid -> int * int
(** [get_dimensions a] returns the dimensions of a *)
