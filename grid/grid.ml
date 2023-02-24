(* Note: comments are written below what they are commenting on *)
module Tile_Rep = struct (* Module for string representations *)
  let a_hidden = "? " (* String representation of a hidden tile *)
  let a_mine = "X " (* String representation of a mine *)
end

module Grid = struct
  open Array
  open Tile_Rep

  let grid_X = 6
  (* Represents the width of the grid *)

  let grid_Y = 6
  (* Represents the height of the grid *)

  type tile =
    | Hidden
    | Mine
    | Empty of int
  (* Represents a tile in the grid *)

  type grid = tile list list

  (* Represents a grid *)
  let preset_grid : grid =
    [
      [ Mine; Empty 2; Empty 2; Empty 2; Empty 2; Mine ];
      [ Empty 2; Empty 3; Mine; Mine; Empty 3; Empty 2 ];
      [ Empty 1; Mine; Empty 3; Empty 4; Mine; Empty 1 ];
      [ Empty 1; Empty 2; Mine; Empty 3; Empty 2; Empty 1 ];
      [ Empty 1; Empty 2; Empty 3; Mine; Empty 1; Empty 0 ];
      [ Empty 1; Mine; Empty 2; Empty 1; Empty 1; Empty 0 ];
    ]
  (* 6x6 2-D list representing the actual grid layout. [[a][b]] represents the
     a'th row and b'th column *)

  let tile_to_string = function
    | Mine -> a_mine
    | Empty a -> string_of_int a ^ " "
    | Hidden -> a_hidden

  (* Helper function that prints out the representation of a tile *)
  let player_grid =
    let array_matrix = Array.make_matrix grid_X grid_Y Hidden in
    let array_of_lists = Array.map Array.to_list array_matrix in
    Array.to_list array_of_lists
  (* Empty grid *)

  let rec display_row (lst : tile list) : string =
    match lst with
    | [] -> ""
    | h :: t -> tile_to_string h ^ display_row t
  (* Returns a string representation of [lst] *)

  let rec display_grid (grid : grid) =
    match grid with
    | [] -> ""
    | h :: t -> display_row h ^ "\n" ^ display_grid t
  (* Returns a string representation of [grid] *)

  let print_grid (grid : grid) = print_endline (display_grid grid)
  (* Prints representation of [grid] *)

  let open_tile ((a,b):int*int) = 
    let _ = assert (a >= 0 && a <= grid_X) in
    let _ = assert (b >= 0 && b <= grid_Y) in
    let new_tile = List.nth (List.nth preset_grid b) a in 
    (* TODO -> Replace player grid with this new_tile *)
  (* Comment out this function above for working code *)
end

let _ = Grid.print_grid Grid.player_grid
