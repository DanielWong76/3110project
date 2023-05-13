open Minesweeper
open Grid

let leaderboard = ref (Leaderboard.empty ())

(* TODO - Add a way to view leaderboard - Make a way to view new leaderboard
   after adding an entry, maybe make it localized? *)

let export_game grid leaderboard file =
  (* Takes in a grid and leaderboard instance and writes it to [file]
     location. *)
  let oc =
    if file = "" then open_out "minesweeper.txt" else open_out (file ^ ".txt")
  in
  let output =
    Grid.export_grid grid
    ^ Leaderboard.export_leaderboard leaderboard
    ^ "File End"
  in
  let file = if file = "" then "minesweeper.txt" else file in
  Printf.fprintf oc "%s\n" output;
  close_out oc;
  print_string ("\nSaved in " ^ file ^ "!\n");
  ()

let rec parse_file ic grid leaderboard =
  let line = input_line ic in
  match line with
  | "File" -> parse_file ic grid leaderboard
  | "Grid" ->
      let new_grid = Grid.import_grid ic in
      parse_file ic new_grid leaderboard
  | "Leaderboard" ->
      let _ = leaderboard := Leaderboard.import_leaderboard ic in
      parse_file ic grid leaderboard
  | "File End" -> grid
  | a -> failwith ("parse_file error: " ^ a)

let import_game file_name =
  (* [import_game file_name] mutates [leaderboard] to be the leaderboard
     contained in [file_name] and returns a grid instance saved in
     [file_name] *)
  let ic = open_in file_name in
  parse_file ic Grid.empty leaderboard

let initialize difficulty =
  match String.lowercase_ascii difficulty with
  | "easy" -> new_grid 8 8 10
  | "medium" -> new_grid 14 14 40
  | "hard" -> new_grid 20 20 99
  | _ -> new_grid 0 0 0

let initialize_custom size =
  match size with
  | x, y, b -> new_grid x y b

let rec choose () : string =
  print_endline "\nPlease choose a difficulty: Easy, Medium, Hard";
  print_string "> ";
  match String.lowercase_ascii (read_line ()) with
  | "easy" -> "Easy"
  | "medium" -> "Medium"
  | "hard" -> "Hard"
  | _ ->
      print_string "Not an Option\n";
      choose ()

let rec custom () =
  print_endline "\nChoose a number of rows and columns (1-40)";
  print_string "> ";
  let x = try read_int () with Failure _ -> -1 in
  if x < 1 || x > 40 then (
    print_string "Not a valid amount\n";
    custom ())
  else (
    print_endline "\nChoose a number of bombs (minimum 1)";
    print_string "> ";
    let b = try read_int () with Failure _ -> -1 in
    if b < 1 || b > (x * x) - 1 then (
      print_string "Not a valid amount\n";
      custom ())
    else (x, x, b))

let make_the_grid yes =
  match yes with
  | "preset" -> initialize (choose ())
  | _ -> initialize_custom (custom ())

let rec custom_difficulty () =
  print_endline "\nCustom difficulty or choose a preset one?";
  print_string "> ";
  match String.lowercase_ascii (read_line ()) with
  | "preset" -> "preset"
  | "custom" -> "custom"
  | _ ->
      print_string "Not an Option\n";
      custom_difficulty ()

let open_tile (i, j) gr =
  try reveal_tile (i, j) gr with
  | Out_of_Bounds ->
      print_string "\nThat square is out of bounds of the grid\n";
      gr
  | Already_Revealed ->
      print_string "\nThat square has already been revealed\n";
      gr
  | Game_Over ->
      print_string
        "\n\
         BOOM! YOU LOSE\n\n\
        \        _ ._  _ , _ ._\n\
        \      (_ ' ( `  )_  .__)\n\
        \    ( (  (    )   `)  ) _)\n\
        \   (__ (_   (_ . _) _) ,__)\n\
        \       `~~` ' . /`~~`\n\
        \            ;   ;\n\
        \            /   \\\n\
         ___________/_ __ \\_____________";
      Grid.empty
  | Win g ->
      print_string "\nYOU WIN!\n ٩(๑･ิᴗ･ิ)۶٩(･ิᴗ･ิ๑)۶\n";
      g

let flagged_tile (i, j) gr =
  try flag_tile (i, j) gr with
  | Out_of_Bounds ->
      print_string "\nThat square is out of bounds of the grid\n";
      gr
  | Already_Revealed ->
      print_string "\nThat square has already been revealed\n";
      gr

let rec choose_action () : string =
  print_endline "Action: Sweep, Flag or Save?\n";
  print_string "> ";
  match String.lowercase_ascii (read_line ()) with
  | "flag" -> "\nFlagging: \n"
  | "sweep" -> "\nSweeping: \n"
  | "save" -> "\nSaving..."
  | _ ->
      print_string "Not an action\n";
      choose_action ()

let rec view_leaderboard () =
  print_endline "\nWould you like to view the leaderboard before you start?";
  print_string "> ";
  match String.lowercase_ascii (read_line ()) with
  | "yes" ->
      print_string
        ("\nLEADERBOARD\n"
       ^ "-------------------------------------------------------"
        ^ Leaderboard.return_top_n 10 !leaderboard
        ^ "\n")
  | "y" ->
      print_string
        ("\nLEADERBOARD\n"
       ^ "-------------------------------------------------------"
        ^ Leaderboard.return_top_n 10 !leaderboard
        ^ "\n")
  | "no" -> ()
  | "n" -> ()
  | _ ->
      print_string "Please type yes or no";
      view_leaderboard ()

let rec get_string () =
  let name = read_line () in
  match name with
  | _ -> (
      print_string "Are you sure? (Y/N)\n";
      print_string "> ";
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
  print_string "> ";
  match String.lowercase_ascii (read_line ()) with
  | "n" -> ()
  | "y" ->
      print_string "What is your name?\n";
      let name = get_string () in
      let _ = print_string "Score saved!\n" in
      let new_score =
        Leaderboard.create_score name
          (calculate_score grid victory)
          (Grid.get_time_taken grid)
          (Grid.get_opened_tiles grid)
      in
      let _ = leaderboard := Leaderboard.add_score new_score !leaderboard in
      print_string
        ("\nLEADERBOARD\n"
       ^ "-------------------------------------------------------"
        ^ Leaderboard.return_top_n 10 !leaderboard
        ^ "\n")
  | _ -> on_game_end grid victory

let rec on_death () =
  print_string "Try again? [Y/N]\n";
  print_string "> ";
  match String.lowercase_ascii (read_line ()) with
  | "yes" -> main ()
  | "y" -> main ()
  | "n" -> print_string "GGs \n"
  | "no" -> print_string "GGs \n"
  | _ ->
      print_string "Sorry, I didn't quite catch that...\n";
      on_death ()

and pictionary () =
  print_string
    "\n\
     Welcome to Pictionary!\n\
     In this mode, we will give you a series of prompts \n\
    \  containing hints as to where mines are placed.\n\
    \  \n\
     At the end you will see the picture that the board forms!\n\n";
  let rec repl gr s =
    print_string s;
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
      repl gr s)
    else (
      print_string "Choose the number column\n";
      print_string "> ";
      let j = try read_int () with Failure _ -> -1 in
      if j < 1 || j > y then (
        print_string "Not a valid column int\n";
        repl gr s)
      else
        let w =
          if action == "\nFlagging: \n" then flagged_tile (i, j) gr
          else open_tile (i, j) gr
        in
        if (w != Grid.empty) then
          if Grid.check_win w then 
            (print_string "\n:)\n";
            Grid.reveal_all_mines w)
          else repl w s 
        else (print_string "\nnt\n"; 
          let rec again () =
            print_string "Try again? [Y/N]\n";
            print_string "> ";
            match String.lowercase_ascii (read_line ()) with
            | "yes" -> repl (reset_grid gr) s
            | "y" -> repl (reset_grid gr) s
            | "n" -> Grid.reveal_all_mines (reset_grid gr)
            | "no" -> Grid.reveal_all_mines (reset_grid gr)
            | _ ->
                print_string "Sorry, I didn't quite catch that...\n";
                again ()
          in again ()))
  in let _ =
    repl Grid.smile "\nPrompt: Be Happy!";
  in
  let rec continue gr s =
    print_string "\nDo you want to try the next level? [Y/N]\n";
    print_string "> ";
    match String.lowercase_ascii (read_line ()) with
    | "n" -> ()
    | "y" -> repl gr s
    | _ ->
        print_string "Choose a valid option\n";
        continue gr s
  in
  let _ = continue Grid.pokeball "\nPrompt: Gotta Catch em All!" in
  continue Grid.charmander "\nPrompt: Catch that Charmander with your pokeball!";
  print_string "\nThanks for playing!\n"

and main () =
  print_string "\n\nWelcome to Minesweeper\n";
  let rec repl gr =
    print_string
      ("\nBombs: "
      ^ string_of_int (Grid.get_mines gr - Grid.get_flagged gr)
      ^ "\n");
    display_grid gr;
    let action = choose_action () in
    if action = "\nSaving..." then
      let file_name =
        print_string "Save name? (leave blank for default)\n";
        print_string "> ";
        get_string ()
      in
      export_game gr !leaderboard file_name
    else
      let _ = print_string action in
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
        if w != Grid.empty then (
          if Grid.check_win w then (
            print_string "\nGood Job!\n";
            on_game_end gr true;
            Grid.reveal_all_mines w;
          ) else
          repl w)
        else (
          print_string "\nGame Over\n";
          on_game_end gr false;
          Grid.reveal_all_mines gr;
          on_death ())
  in
  let rec game_mode () =
    print_string "\nPlease Choose a GameMode: Classic, Pictionary\n";
    print_string "> ";
    let i = read_line () in
    match String.lowercase_ascii i with
    | "classic" ->
        let _ = view_leaderboard () in
        repl (make_the_grid (custom_difficulty ()))
    | "p" -> pictionary ()
    | "pictionary" -> pictionary ()
    | _ ->
        print_string "\nNot a valid Game Mode\n";
        game_mode ()
  in
  let rec resume_or_save () =
    print_string
      "\nWould you like to start a new game or resume your old one?\n";
    print_string "\nChoose: New, Resume\n";
    print_string "> ";
    let i = read_line () in
    match String.lowercase_ascii i with
    | "resume" ->
        print_string
          "\nPlease insert save name to load or leave blank for default.\n";
        print_string "> ";
        let i = read_line () in
        if String.lowercase_ascii i = "" then
          repl (import_game "minesweeper.txt")
        else repl (import_game (i ^ ".txt"))
    | "new" -> game_mode ()
    | _ ->
        print_string "\nNot an option\n";
        resume_or_save ()
  in
  resume_or_save ()

(* Execute the game engine. *)
let () = main ()
