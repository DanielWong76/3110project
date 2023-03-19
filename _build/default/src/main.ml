open Grid

let initialize difficulty = match difficulty with
  | "Easy" -> new_grid 8 8 10
  | "Medium" -> new_grid 13 13 40
  | "Hard" -> new_grid 16 30 99
  | _ -> new_grid 0 0 0

let main () =  
  print_string "\n\nWelcome to Mine Sweeper\n";
  let rec choose () : string = 
    print_endline "Please choose a difficulty: Easy, Medium, Hard\n";
    print_string "> ";
    match read_line () with 
      | "Easy" -> "Easy"
      | "Medium" -> "Medium"
      | "Hard" -> "Hard"
      | _ -> print_string "Not an Option\n"; choose ();
  in let rec repl gr = 
    display_grid gr;
    print_string "Choose the number row to sweep\n";
    print_string "> ";
    let x = match get_dimensions gr with
      | (c,_) -> c
    in let y = match get_dimensions gr with
      | (_,d) -> d
    in let i = try read_int () with
          | Failure _ ->  -1
        in if i < 1 || i>x then (print_string "Not a valid row int\n"; repl gr) else
          print_string "Choose the number column to sweep\n";
          print_string "> ";
          let j = try read_int () with
          | Failure _ ->  -1
          in if j < 1 || j>y then (print_string "Not a valid column int\n"; repl gr) else
            let w = try reveal_tile (i,j) gr with
              | Out_of_Bounds -> print_string "\nThat square is out of bounds of the grid\n"; gr
              | Already_Revealed -> print_string "\nThat square has already been revealed\n"; gr
              | Game_Over -> print_string "\nBOOM! YOU LOSE\n"; empty
            in if w != empty then repl w else (print_string "\nGame Over\n"; raise End_of_file)
        in repl (initialize (choose ()))
let () = main ()