type grid = {
  mines : (int * int) list; (* (a,b) represents the coordinates of a mine *)
  opened : (int * int) list;
      (* (a,b) indicates where the player has revealed a tile *)
  flagged : (int * int) list;
  dimensions : int * int; (* (a,b) represents the grid is of size a*b *)
  tiles_opened : int; (* The number of tiles opened in this grid *)
  time_taken : float; (* Time elapsed since this grid's creation *)
  time_created : float;
}

exception Out_of_Bounds
exception Already_Revealed
exception Game_Over
exception Win of grid

let empty : grid =
  {
    mines = [];
    opened = [];
    flagged = [];
    dimensions = (0, 0);
    tiles_opened = 0;
    time_taken = 0.;
    time_created = Unix.time ();
  }

let unrevealed = "?"
let mine = "\027[31mX\027[37m"
let flagged = "\027[31mF\027[37m"
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
  let _ = Random.self_init in
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
    tiles_opened = 0;
    time_taken = 0.;
    time_created = Unix.time ();
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

let rec fold_grid f (grid : grid) (coord_list : (int * int) list) =
  match coord_list with
  | [] -> grid
  | (a, b) :: t ->
      if List.mem (a, b) grid.opened then fold_grid f grid t
      else fold_grid f (f (a, b) grid) t

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

let update_time grid =
  { grid with time_taken = Unix.time () -. grid.time_created }

let rec reveal_tile (coords : int * int) (grid : grid) =
  let grid = update_time grid in
  if List.mem coords grid.opened then
    raise Already_Revealed (* Checks if already opened tile *)
  else if List.mem coords grid.flagged then
    match coords with
    | x, y ->
        let new_flagged =
          List.filter (fun (a, b) -> a != x && b != y) grid.flagged
        in
        { grid with flagged = new_flagged }
  else
    let new_opened = coords :: grid.opened in
    if List.mem coords grid.mines then
      raise Game_Over (* Checks if opened tile is a mine*)
    else
      let grid =
        if List.mem coords grid.flagged then flag_tile coords grid else grid
      in
      if determine_num grid coords = 0 then
        match coords with
        | a, b ->
            let x = get_dimensions_x grid in
            let y = get_dimensions_y grid in
            let surroundings =
              (* Gets surrounding tiles *)
              [
                (Int.max (a - 1) 1, Int.max (b - 1) 1);
                (a, Int.max (b - 1) 1);
                (Int.min (a + 1) x, Int.max (b - 1) 1);
                (Int.max (a - 1) 1, b);
                (Int.min (a + 1) x, b);
                (Int.max (a - 1) 1, Int.min (b + 1) y);
                (a, Int.min (b + 1) y);
                (Int.min (a + 1) y, Int.min (b + 1) y);
              ]
            in
            fold_grid reveal_tile
              {
                grid with
                opened = new_opened;
                tiles_opened = grid.tiles_opened + 1;
              }
              (List.sort_uniq
                 (fun (i, j) (x, y) ->
                   if i > x then 1 else if i = x && j = y then 0 else -1)
                 surroundings)
      else
        match grid.dimensions with
        | x, y ->
            if grid.tiles_opened + 1 = (x * y) - List.length grid.mines then
              raise
                (Win
                   {
                     grid with
                     opened = new_opened;
                     tiles_opened = grid.tiles_opened + 1;
                   })
            else
              {
                grid with
                opened = new_opened;
                tiles_opened = grid.tiles_opened + 1;
              }

let rec copy_string num string acc =
  if num <= 0 then acc else copy_string (num - 1) string (acc ^ string)

(* Adds the column coordinates to the string rep of the grid. Call this After
   the end of the print_coord function *)
let add_coord_numbers_to_grid (grid : grid) : string =
  let rec add_numbers curr num_columns =
    (* Adds the numbers and a newline *)
    if curr = num_columns then string_of_int curr ^ "\n"
    else
      let max_digits =
        float_of_int num_columns |> Float.log10 |> int_of_float |> ( + ) 1
      in
      let current_digits =
        float_of_int (curr + 1) |> Float.log10 |> int_of_float |> ( + ) 1
      in
      let num_spaces = max_digits - current_digits + 1 in
      string_of_int curr
      ^ copy_string num_spaces " " ""
      ^ add_numbers (curr + 1) num_columns
  in
  let rec add_dashes curr num_columns =
    if curr > num_columns then "\n"
    else
      let max_digits =
        float_of_int num_columns |> Float.log10 |> int_of_float |> ( + ) 2
      in
      copy_string max_digits "-" "" ^ add_dashes (curr + 1) num_columns
  in
  let dimensions = grid.dimensions in
  match dimensions with
  | a, b ->
      let row_digits = int_of_float (Float.log10 (float_of_int a)) + 1 in
      copy_string (row_digits + 4) " " ""
      ^ add_numbers 1 b
      ^ copy_string (row_digits + 3) " " ""
      ^ add_dashes 1 b

(* Processes the spacing of numbers in the display mode for rows *)
let process_number num columns =
  let fnum = float_of_int num in
  let fcolumns = float_of_int columns in
  let fnum_digits = int_of_float (Float.log10 fnum) + 1 in
  let fcolumns_digits = int_of_float (Float.log10 fcolumns) + 1 in
  let spaces_needed = fcolumns_digits - fnum_digits in
  copy_string spaces_needed " " "" ^ string_of_int num

let color_number num =
  match num with
  | 0 -> "\027[30m"
  | 1 -> "\027[36m"
  | 2 -> "\027[32m"
  | 3 -> "\027[35m"
  | 4 -> "\027[33m"
  | 5 -> "\027[34m"
  | 6 -> "\027[34m"
  | 7 -> "\027[34m"
  | 8 -> "\027[34m"
  | _ -> "\027[37m"

let rec print_coord (grid : grid) (row : int) (column : int)
    (reveal_all_mines : bool) =
  let coord = (row, column) in
  let curr =
    if List.mem coord grid.flagged then flagged
    else if reveal_all_mines && List.mem coord grid.mines then mine
    else if List.mem coord grid.opened then
      if List.mem coord grid.mines then mine
      else
        color_number (determine_num grid coord)
        ^ string_of_int (determine_num grid coord)
        ^ "\027[37m"
    else unrevealed
  in
  let columns =
    match grid.dimensions with
    | _, b -> b
  in
  let spaces =
    float_of_int (1 + columns) |> Float.log10 |> int_of_float |> ( + ) 1
  in
  let curr = curr ^ copy_string spaces " " "" in
  let string_rep_of_grid =
    match grid.dimensions with
    | a, b ->
        if a = column then
          if b = row then curr
          else
            curr ^ "\n "
            ^ process_number (row + 1) b
            ^ " | "
            ^ print_coord grid (row + 1) 1 reveal_all_mines
        else curr ^ print_coord grid row (column + 1) reveal_all_mines
  in
  "" ^ string_rep_of_grid

let display_grid (grid : grid) =
  match grid.dimensions with
  | _, b ->
      add_coord_numbers_to_grid grid
      ^ " " ^ process_number 1 b ^ " | " ^ print_coord grid 1 1 false
      |> print_endline
(* let _ = new_grid 5 5 5 |> reveal_tile (3, 3) |> display_grid *)

let reveal_all_mines (grid : grid) =
  match grid.dimensions with
  | _, b ->
      add_coord_numbers_to_grid grid
      ^ " " ^ process_number 1 b ^ " | " ^ print_coord grid 1 1 true
      |> print_endline

let get_opened_tiles grid = grid.tiles_opened
let add_time grid time = { grid with time_taken = grid.time_taken +. time }
let get_time_taken grid = grid.time_taken

let rec list_to_string lst =
  match lst with
  | [] -> ""
  | h :: t -> (
      match h with
      | x, y ->
          string_of_int x ^ " " ^ string_of_int y ^ "\n" ^ list_to_string t)

let export_grid grid =
  let mines = "Mines\n" ^ list_to_string grid.mines in
  let opened = "Opened\n" ^ list_to_string grid.opened in
  let flagged = "Flagged\n" ^ list_to_string grid.flagged in
  let dimensions =
    match grid.dimensions with
    | x, y -> "Dimensions\n" ^ string_of_int x ^ string_of_int y
  in
  let tiles_opened = "Tiles Opened\n" ^ string_of_int grid.tiles_opened in
  let time_taken = "Time Taken\n" ^ string_of_float grid.time_taken in
  let time_created = "Time Created\n" ^ string_of_float grid.time_created in
  mines ^ opened ^ flagged ^ dimensions ^ tiles_opened ^ time_taken
  ^ time_created

let check_win grid =
  match grid.dimensions with
  | x, y -> grid.tiles_opened = (x * y) - List.length grid.mines

let _ = export_grid empty
