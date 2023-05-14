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
  let _ = Random.self_init () in
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
  let mines =
    "Mines\n"
    ^ string_of_int (List.length grid.mines)
    ^ "\n" ^ list_to_string grid.mines
  in
  let opened =
    "Opened\n"
    ^ string_of_int (List.length grid.opened)
    ^ "\n" ^ list_to_string grid.opened
  in
  let flagged =
    "Flagged\n"
    ^ string_of_int (List.length grid.flagged)
    ^ "\n"
    ^ list_to_string grid.flagged
  in
  let dimensions =
    match grid.dimensions with
    | x, y -> "Dimensions\n" ^ string_of_int x ^ " " ^ string_of_int y
  in
  let tiles_opened = "\nTiles Opened\n" ^ string_of_int grid.tiles_opened in
  let time_taken = "\nTime Taken\n" ^ string_of_float grid.time_taken in
  let time_created = "\nTime Created\n" ^ string_of_float grid.time_created in
  "Grid\n" ^ mines ^ opened ^ flagged ^ dimensions ^ tiles_opened ^ time_taken
  ^ time_created ^ "\nGrid End\n"

let check_win grid =
  match grid.dimensions with
  | x, y -> grid.tiles_opened = (x * y) - List.length grid.mines

let smile =
  {
    mines =
      [
        (2, 3);
        (3, 3);
        (4, 3);
        (2, 6);
        (3, 6);
        (4, 6);
        (6, 2);
        (6, 7);
        (7, 2);
        (7, 3);
        (7, 4);
        (7, 5);
        (7, 6);
        (7, 7);
      ];
    opened = [];
    flagged = [];
    dimensions = (8, 8);
    tiles_opened = 0;
    time_taken = 0.;
    time_created = Unix.time ();
  }

let pokeball =
  {
    mines =
      [
        (1, 6);
        (1, 7);
        (1, 8);
        (1, 9);
        (2, 4);
        (2, 5);
        (2, 10);
        (2, 11);
        (3, 3);
        (3, 12);
        (4, 2);
        (4, 13);
        (5, 2);
        (5, 13);
        (6, 1);
        (6, 7);
        (6, 8);
        (6, 14);
        (7, 1);
        (7, 2);
        (7, 3);
        (7, 4);
        (7, 5);
        (7, 6);
        (7, 9);
        (7, 10);
        (7, 11);
        (7, 12);
        (7, 13);
        (7, 14);
        (8, 1);
        (8, 2);
        (8, 3);
        (8, 4);
        (8, 5);
        (8, 6);
        (8, 9);
        (8, 10);
        (8, 11);
        (8, 12);
        (8, 13);
        (8, 14);
        (14, 6);
        (14, 7);
        (14, 8);
        (14, 9);
        (13, 4);
        (13, 5);
        (13, 10);
        (13, 11);
        (12, 3);
        (12, 12);
        (11, 2);
        (11, 13);
        (10, 2);
        (10, 13);
        (9, 1);
        (9, 7);
        (9, 8);
        (9, 14);
      ];
    opened = [];
    flagged = [];
    dimensions = (14, 14);
    tiles_opened = 0;
    time_taken = 0.;
    time_created = Unix.time ();
  }

let charmander = {
  mines = [
          (2,5);
          (2,6);
          (2,7);
          (2,8);
          (2,18);
          (3,4);
          (3,9);
          (3,17);
          (3,19);
          (4,3);
          (4,10);
          (4,17);
          (4,20);
          (5,3);
          (5,10);
          (5,17);
          (5,20);
          (6,2);
          (6,11);
          (6,16);
          (6,21);
          (7,1);
          (7,7);
          (7,11);
          (7,16);
          (7,21);
          (8,1);
          (8,6);
          (8,7);
          (8,12);
          (8,16);
          (8,21);
          (9,1);
          (9,6);
          (9,7);
          (9,12);
          (9,17);
          (9,19);
          (9,20);
          (10,2);
          (10,13);
          (10,17);
          (10,19);
          (11,3);
          (11,4);
          (11,14);
          (11,16);
          (11,19);
          (12,5);
          (12,6);
          (12,7);
          (12,10);
          (12,14);
          (12,15);
          (12,18);
          (13,6);
          (13,9);
          (13,15);
          (13,18);
          (14,6);
          (14,10);
          (14,11);
          (14,15);
          (14,17);
          (15,5);
          (15,7);
          (15,15);
          (15,16);
          (16,6);
          (16,7);
          (16,8);
          (17,14);
          (17,15);
          (18,9);
          (18,10);
          (18,11);
          (18,13);
          (18,14);
          (19,10);
          (19,14);
          (20,11);
          (20,12);
          (20,13);
          (20,14);
          ];
  opened = [];
  flagged = [];
  dimensions = (21,21);
  tiles_opened = 0;
  time_taken = 0.;
  time_created = Unix.time ();
}

let is_grid_keyword str =
  match str with
  | "Mines" -> true
  | "Opened" -> true
  | "Flagged" -> true
  | "Dimensions" -> true
  | "Tiles Opened" -> true
  | "Time Taken" -> true
  | "Time Created" -> true
  | "Grid End" -> true
  | "File End" -> true
  | _ -> false

let list_to_tuple lst =
  (* Converts a list of two elements into a tuple *)
  let x = int_of_string (List.hd lst) in
  let y = int_of_string (List.nth lst 1) in
  (x, y)

let rec parse_coordinates (acc : (int * int) list) ic num =
  (* Returns a (string*string) list of coordinates from [ic] up until a grid
     keyword *)
  if num <= 0 then acc
  else
    try
      let line = input_line ic in
      if is_grid_keyword line then acc
      else
        let raw = String.split_on_char ' ' line in
        let coordinates = list_to_tuple raw in
        let new_acc = coordinates :: acc in
        parse_coordinates new_acc ic (num - 1)
    with e ->
      close_in_noerr ic;
      raise e

(* let parse_single_line ic = try let line = input_line ic in if is_grid_keyword
   line then failwith ("Unexpected Keyword: " ^ line) else let raw =
   String.split_on_char ' ' line in list_to_tuple raw with e -> close_in_noerr
   ic; raise e *)

let parse_mines ic grid =
  try
    let line = int_of_string (input_line ic) in
    let mines_loc = parse_coordinates [] ic line in
    { grid with mines = mines_loc }
  with e ->
    close_in_noerr ic;
    raise e

let parse_opened ic grid =
  try
    let line = int_of_string (input_line ic) in
    let opened_loc = parse_coordinates [] ic line in
    { grid with opened = opened_loc }
  with e ->
    close_in_noerr ic;
    raise e

let parse_flagged ic grid =
  try
    let line = int_of_string (input_line ic) in
    let flagged_loc = parse_coordinates [] ic line in
    { grid with flagged = flagged_loc }
  with e ->
    close_in_noerr ic;
    raise e

let rec parse_keywords ic grid =
  try
    let keyword = input_line ic in
    match keyword with
    | "Mines" -> parse_keywords ic (parse_mines ic grid)
    | "Opened" -> parse_keywords ic (parse_opened ic grid)
    | "Flagged" -> parse_keywords ic (parse_flagged ic grid)
    | "Dimensions" ->
        let line = input_line ic in
        if is_grid_keyword line then failwith "Unexpected Keyword"
        else
          let coords = String.split_on_char ' ' line in
          let formatted = list_to_tuple coords in
          parse_keywords ic { grid with dimensions = formatted }
    | "Tiles Opened" ->
        let line = input_line ic in
        if is_grid_keyword line then failwith "Unexpected Keyword"
        else parse_keywords ic { grid with tiles_opened = int_of_string line }
    | "Time Taken" ->
        let line = input_line ic in
        if is_grid_keyword line then failwith "Unexpected Keyword"
        else parse_keywords ic { grid with time_taken = float_of_string line }
    | "Time Created" ->
        let line = input_line ic in
        if is_grid_keyword line then failwith "Unexpected Keyword"
        else parse_keywords ic { grid with time_taken = float_of_string line }
    | "Grid End" -> grid
    | a -> failwith ("Grid Loading Error: " ^ a)
  with e ->
    close_in_noerr ic;
    raise e

let reset_grid gr = {mines=gr.mines; 
                    opened=[]; 
                    flagged=[]; 
                    dimensions=gr.dimensions; 
                    tiles_opened=0; 
                    time_taken=0.; 
                    time_created=Unix.time ();}

let get_opened_tiles_list gr = gr.opened

let import_grid ic =
  let new_grid = empty in
  parse_keywords ic new_grid
