(** Representation and control of the minesweeper grid *)

type grid
(** The type [grid] represents the minesweeper grid *)

val empty : grid
(** Returns the representation of an empty grid *)

exception Out_of_Bounds
(** Raised when coords given are out of bounds for the given grid *)

exception Already_Revealed
(** Raised when [reveal_tile] attempts to reveal an already opened tile *)

exception Game_Over
(** Raised when [reveal_tile] opens a mine *)

exception Win of grid
(** Raised when [reveal_tile] causes a win and returns winning grid*)

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
(** [print_coord gr x y b] prints the tile of gr at (x,y) if b is false. If b is
    true, reveals all tiles*)

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

val determine_num : grid -> int * int -> int
(** [determine_num gr i] returns the number of the tile at i on gr *)

val reveal_all_mines : grid -> unit
(** [reveal_all_mines g] prints g with all player progress and mines revealed *)

val get_opened_tiles : grid -> int
(** [get_opened_tiles g] returns the number of opened tiles in [g] *)

val get_opened_tiles_list : grid -> (int * int) list
(** [get_opened_tiles_list g] returns the list of coordinates of tiles that 
    have been opened in g*)

val add_time : grid -> float -> grid
(** [add_time g t] returns a grid with the time of [t] to [g.time_taken] *)

val get_time_taken : grid -> float
(** [get_time_taken gr] gets the amount of time taken for a grid gr*)

val update_time : grid -> grid
(** [update_time g] returns a grid with time updated from now. *)

val export_grid : grid -> string
(** [export_grid g] returns a string representation of the information of [g] to
    be printed out in a file*)

val check_win : grid -> bool
(** [check_win g] checks if grid g has won*)

val import_grid : in_channel -> grid
(** [import_grid ic] returns a grid formed by the contents of [ic] *)

val smile : grid
(** A grid of a smile :)*)

val pokeball : grid
(** A grid of a pokeball*)

val charmander : grid
(** A grid of a charmander*)

val unrevealed : string
(** Representation of unrevealed tiles *)

val mine : string
(** Representation of mines *)

val flagged : string
(** Representation of flagged tile*)

val generate_mines : int -> (int * int) list -> int -> int -> (int * int) list
(** [generate_mines a b x y] generates a mines in an x by y grid. Returns a list
    of the coordinates of all the mines generated *)

val check_mines : grid -> (int * int) list -> int
(** [check_mine g lst] checks how many mines in [lst] are in [g] *)

val add_coord_numbers_to_grid : grid -> string
(** Adds the column coordinates to the string rep of the grid. Call this After
    the end of the print_coord function *)

val reset_grid : grid -> grid
(** [reset_grid gr] returns gr with flagged and opened mines reset*)
