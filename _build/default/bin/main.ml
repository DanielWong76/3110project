open Minesweeper
open Grid

let leaderboard = ref (Leaderboard.empty ())

(* TODO - Add a way to view leaderboard - Make a way to view new leaderboard
   after adding an entry, maybe make it localized? *)

let initialize difficulty =
  match String.lowercase_ascii difficulty with
  | "easy" -> new_grid 8 8 10
  | "medium" -> new_grid 14 14 40
  | "hard" -> new_grid 20 20 99
  | _ -> new_grid 0 0 0

let rec choose () : string =
  print_endline "Please choose a difficulty: Easy, Medium, Hard\n";
  print_string "> ";
  match String.lowercase_ascii (read_line ()) with
  | "easy" -> "Easy"
  | "medium" -> "Medium"
  | "hard" -> "Hard"
  | _ ->
      print_string "Not an Option\n";
      choose ()

let open_tile (i, j) gr =
  try reveal_tile (i, j) gr with
  | Out_of_Bounds ->
      print_string "\nThat square is out of bounds of the grid\n";
      gr
  | Already_Revealed ->
      print_string "\nThat square has already been revealed\n";
      gr
  | Game_Over ->
      print_string "\nBOOM! YOU LOSE\n";
      Grid.empty

let flagged_tile (i, j) gr =
  try flag_tile (i, j) gr with
  | Out_of_Bounds ->
      print_string "\nThat square is out of bounds of the grid\n";
      gr
  | Already_Revealed ->
      print_string "\nThat square has already been revealed\n";
      gr

let rec choose_action () : string =
  print_endline "Action: Sweep or Flag?\n";
  print_string "> ";
  match String.lowercase_ascii (read_line ()) with
  | "flag" -> "\nFlagging: \n"
  | "sweep" -> "\nSweeping: \n"
  | _ ->
      print_string "Not an action\n";
      choose_action ()

let rec get_string () =
  let name = read_line () in
  match name with
  | _ -> (
      print_string "Are you sure? (Y/N)\n";
      match String.lowercase_ascii (read_line ()) with
      | "y" -> name
      | _ -> get_string ())

let calculate_score grid win =
  (* Returns the score of [grid] *)
  if win then
    let tiles_score = Grid.get_opened_tiles grid in
    let time_score = int_of_float (300. -. Grid.get_time_taken grid) in
    let dimensions = Grid.get_dimensions grid in
    match dimensions with
    | x, y ->
        let difficulty_score = x * y in
        tiles_score + time_score + difficulty_score
  else Grid.get_opened_tiles grid

let rec on_game_end grid victory =
  print_string ("Score: " ^ string_of_int (calculate_score grid victory) ^ "\n");
  print_string "Would you like to save your score? (Y/N)\n";
  match String.lowercase_ascii (read_line ()) with
  | "n" -> ()
  | "y" ->
      print_string "What is your name?\n";
      let name = get_string () in
      let _ = print_string "Score saved!" in
      let new_score =
        Leaderboard.create_score name
          (calculate_score grid victory)
          (Grid.get_time_taken grid)
          (Grid.get_opened_tiles grid)
      in
      let _ = leaderboard := Leaderboard.add_score new_score !leaderboard in
      print_string (Leaderboard.return_top_n 10 !leaderboard)
  | _ -> on_game_end grid victory

let rec on_death () =
  print_string "Try again? (Y/N)\n";
  match String.lowercase_ascii (read_line ()) with
  | "y" -> main ()
  | "n" -> raise End_of_file
  | _ ->
      print_string "Sorry, I didn't quite catch that...\n";
      on_death ()

and main () =
  print_string "\n\nWelcome to Minesweeper\n";
  let rec repl gr =
    print_string
      ("\nBombs: "
      ^ string_of_int (Grid.get_mines gr - Grid.get_flagged gr)
      ^ "\n");
    display_grid gr;
    let action = choose_action () in
    print_string action;
    print_string "Choose the number row\n";
    print_string "> ";
    let x = Grid.get_dimensions_x gr in
    let y = Grid.get_dimensions_y gr in
    let i = try read_int () with Failure _ -> -1 in
    if i < 1 || i > x then (
      print_string "Not a valid row int\n";
      repl gr)
    else print_string "Choose the number column\n";
    print_string "> ";
    let j = try read_int () with Failure _ -> -1 in
    if j < 1 || j > y then (
      print_string "Not a valid column int\n";
      repl gr)
    else
      let w =
        if action == "\nFlagging: \n" then flagged_tile (i, j) gr
        else open_tile (i, j) gr
      in
      if w != Grid.empty then repl w
      else (
        print_string "\nGame Over\n";
        on_game_end gr false;
        Grid.reveal_all_mines gr;
        on_death ())
  in
  repl (initialize (choose ()))

(* Execute the game engine. *)
let () = main ()
