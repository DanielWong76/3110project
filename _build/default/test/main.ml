open OUnit2
open Minesweeper
open Grid

(* let new_grid_test a b c (expected_output : grid) = "new_grid test" >:: fun _
   -> assert_equal expected_output (new_grid a b c) *)

let get_dimensions_test grid (expected_output : int * int) =
  "get_dimensions test" >:: fun _ ->
  assert_equal expected_output (get_dimensions grid)

let get_dimensions_test_x grid (expected_output : int) =
  "get_dimensions_x test" >:: fun _ ->
  assert_equal expected_output (get_dimensions_x grid)

let get_dimensions_test_y grid (expected_output : int) =
  "get_dimensions_y test" >:: fun _ ->
  assert_equal expected_output (get_dimensions_y grid)

let get_mines_test grid (expected_output : int) =
  "get_mines test" >:: fun _ -> assert_equal expected_output (get_mines grid)

let grid_tests =
  [
    get_dimensions_test (new_grid 4 5 0) (4, 5);
    get_dimensions_test (new_grid 8 10 6) (8, 10);
    get_dimensions_test (new_grid 3 3 3) (3, 3);
    get_dimensions_test_x (new_grid 4 5 0) 4;
    get_dimensions_test_x (new_grid 7 5 5) 7;
    get_dimensions_test_x (new_grid 0 0 0) 0;
    get_dimensions_test_y (new_grid 0 0 0) 0;
    get_dimensions_test_y (new_grid 8 8 0) 8;
    get_dimensions_test_y (new_grid 7 5 5) 5;
    get_mines_test (new_grid 8 8 10) 10;
    get_mines_test (new_grid 14 14 40) 40;
    get_mines_test (new_grid 10 10 0) 0;
  ]

let suite = "test suite for minesweeper" >::: List.flatten [ grid_tests ]
let _ = run_test_tt_main suite
