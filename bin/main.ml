open Minesweeper
open Grid

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
      empty

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
      if w != empty then repl w
      else (
        print_string "\nGame Over\n";
        Grid.reveal_all_mines gr;
        on_death ())
  in
  repl (initialize (choose ()))

(* Execute the game engine. *)
let () = main ()
