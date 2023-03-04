(** Representation and control of the minesweeper grid *)

type grid
(** The type [grid] represents the minesweeper grid *)

exception Out_of_Bounds
(** Raised when coords given are out of bounds for the given grid *)

val new_grid : int -> int -> grid
(** [new grid a b] returns a new random grid of dimensions axb *)

val reveal_tile : int * int -> grid -> _
(** [reveal_tile (a,b) c] opens the tile at location (a,b) in grid c and
    processes the corresponding action (ex. hitting a mine).

    Raises: [Out_of_Bounds] if (a,b) is not contained in c *)

val display_grid : grid -> _
(** [display_grid a] prints a to the command line interface *)
