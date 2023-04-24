type grid = {
  mines : (int * int) list; (* (a,b) represents the coordinates of a mine *)
  opened : (int * int) list;
      (* (a,b) indicates where the player has revealed a tile *)
  flagged : (int * int) list;
  dimensions : int * int; (* (a,b) represents the grid is of size a*b *)
}

exception Out_of_Bounds
exception Already_Revealed
exception Game_Over

let empty : grid =
  { mines = []; opened = []; flagged = []; dimensions = (0, 0) }

let unrevealed = "? "
let mine = "X "
let flagged = "F "
let get_dimensions (grid : grid) = grid.dimensions

let get_dimensions_x (grid : grid) =
  match grid.dimensions with
  | x, _ -> x

let get_dimensions_y (grid : grid) =
  match grid.dimensions with
  | _, y -> y

let get_mines (grid : grid) = List.length grid.mines
let get_flagged (grid : grid) = List.length grid.flagged

let rec generate_mines (mine_num : int) (mines : (int * int) list)
    (max_row : int) (max_column : int) =
  if mine_num = 0 then mines
  else
    let new_mine = (Random.int max_column + 1, Random.int max_row + 1) in
    if List.mem new_mine mines then
      generate_mines mine_num mines max_row max_column
    else generate_mines (mine_num - 1) (new_mine :: mines) max_row max_column

let new_grid (rows : int) (columns : int) (num_mines : int) =
  {
    mines = generate_mines num_mines [] rows columns;
    (* TODO: Add some way of adding mines*)
    opened = [];
    flagged = [];
    dimensions = (rows, columns);
  }

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

let flag_tile (coords : int * int) (grid : grid) =
  if List.mem coords grid.opened then
    raise Already_Revealed (* Checks if already opened tile *)
  else
    let x =
      match coords with
      | x, _ -> x
    in
    let y =
      match coords with
      | _, y -> y
    in
    if List.mem coords grid.flagged then
      {
        grid with
        flagged = List.filter (fun (a, b) -> a != x && b != y) grid.flagged;
      }
    else { grid with flagged = coords :: grid.flagged }

let reveal_tile (coords : int * int) (grid : grid) =
  if List.mem coords grid.opened then
    raise Already_Revealed (* Checks if already opened tile *)
  else
    let grid =
      if List.mem coords grid.flagged then flag_tile coords grid else grid
    in
    let new_opened = coords :: grid.opened in
    if List.mem coords grid.mines then
      raise Game_Over (* Checks if opened tile is a mine*)
    else { grid with opened = new_opened }

let rec print_coord (grid : grid) (row : int) (column : int) =
  let coord = (row, column) in
  let curr =
    if List.mem coord grid.flagged then flagged
    else if List.mem coord grid.opened then
      if List.mem coord grid.mines then mine
      else string_of_int (determine_num grid coord) ^ " "
    else unrevealed
  in
  match grid.dimensions with
  | a, b ->
      if a = column then
        if b = row then curr else curr ^ "\n" ^ print_coord grid (row + 1) 1
      else curr ^ print_coord grid row (column + 1)

let display_grid (grid : grid) = print_coord grid 1 1 |> print_endline
(* let _ = new_grid 5 5 5 |> reveal_tile (3, 3) |> display_grid *)
