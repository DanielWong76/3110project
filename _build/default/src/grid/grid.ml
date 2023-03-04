type grid = {
  mines : (int * int) list; (* (a,b) represents the coordinates of a mine *)
  opened : (int * int) list;
      (* (a,b) indicates where the player has revealed a tile *)
  dimensions : int * int; (* (a,b) represents the grid is of size a*b *)
}

exception Out_of_Bounds
exception Already_Revealed
exception Game_Over

let unrevealed = "?"
let mine = "X"

let new_grid (rows : int) (columns : int) =
  {
    mines = [];
    (* TODO: Add some way of adding mines*)
    opened = [];
    dimensions = (rows, columns);
  }

let reveal_tile (coords : int * int) (grid : grid) =
  if List.mem coords grid.opened then
    raise Already_Revealed (* Checks if already opened tile *)
  else
    let new_opened = coords :: grid.opened in
    if List.mem coords grid.mines then
      raise Game_Over (* Checks if opened tile is a mine*)
    else { grid with opened = new_opened }

let get_dimensions (grid : grid) = grid.dimensions

(** [check_mine g lst] checks how many mines in [lst] are in [g] *)
let check_mines (grid : grid) (coords : (int * int) list) =
  List.fold_left
    (fun acc (a, b) -> if List.mem (a, b) grid.mines then acc + 1 else acc)
    0 coords

let determine_num (grid : grid) (coords : int * int) =
  match coords with
  | a, b ->
      let surroundings =
        (* Gets surrounding tiles *)
        [
          (a - 1, b - 1);
          (a, b - 1);
          (a + 1, b - 1);
          (a - 1, b);
          (a + 1, b);
          (a - 1, b + 1);
          (a, b + 1);
          (a + 1, b + 1);
        ]
      in
      check_mines grid surroundings

let rec print_coord (grid : grid) (row : int) (column : int) =
  let coord = (row, column) in
  let curr =
    if List.mem coord grid.mines then mine
    else if List.mem coord grid.opened then
      string_of_int (determine_num grid coord)
    else unrevealed
  in
  match grid.dimensions with
  | a, b ->
      if a = column then
        if b = row then curr else curr ^ "\n" ^ print_coord grid (row + 1) 1
      else curr ^ print_coord grid row (column + 1)

let display_grid (grid : grid) = print_coord grid 1 1 |> print_endline
let _ = new_grid 5 5 |> display_grid
