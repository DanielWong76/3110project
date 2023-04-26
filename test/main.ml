open OUnit2
open Minesweeper
open Grid

let get_dimensions_test (description : string) (grid : grid)
    (expected_output : int * int) =
  description >:: fun _ -> assert_equal expected_output (get_dimensions grid)

let get_dimensions_test_x (description : string) (grid : grid)
    (expected_output : int) =
  description >:: fun _ -> assert_equal expected_output (get_dimensions_x grid)

let get_dimensions_test_y (description : string) (grid : grid)
    (expected_output : int) =
  description >:: fun _ -> assert_equal expected_output (get_dimensions_y grid)

let get_mines_test (description : string) (grid : grid) (expected_output : int)
    =
  description >:: fun _ -> assert_equal expected_output (get_mines grid)

let get_opened_tiles_test (description : string) (grid : grid)
    (expected_output : int) =
  description >:: fun _ -> assert_equal expected_output (get_opened_tiles grid)

let get_flagged_test (description : string) (grid : grid)
    (expected_output : int) =
  description >:: fun _ -> assert_equal expected_output (get_flagged grid)

let grid_tests =
  [
    get_dimensions_test "get_dimensions test for 4x5" (new_grid 4 5 0) (4, 5);
    get_dimensions_test "get_dimensions test for 8x10" (new_grid 8 10 6) (8, 10);
    get_dimensions_test "get_dimensions test for 3x3" (new_grid 3 3 3) (3, 3);
    get_dimensions_test_x "get_dimensions_x test for 4x5" (new_grid 4 5 0) 4;
    get_dimensions_test_x "get_dimensions_x test for 7x5" (new_grid 7 5 5) 7;
    get_dimensions_test_x "get_dimensions_x test for 0x0" (new_grid 0 0 0) 0;
    get_dimensions_test_y "get_dimensions_y test for 0x0" (new_grid 0 0 0) 0;
    get_dimensions_test_y "get_dimensions_y test for 8x8" (new_grid 8 8 0) 8;
    get_dimensions_test_y "get_dimensions_y test for 7x5" (new_grid 7 5 5) 5;
    get_mines_test "get_mines test for 8x8 w/ 10 mines" (new_grid 8 8 10) 10;
    get_mines_test "get_mines test for 14x14 w/ 40 mines" (new_grid 14 14 40) 40;
    get_mines_test "get_mines test for no mines" (new_grid 10 10 0) 0;
    get_mines_test "get_mines test for all mines" (new_grid 5 5 25) 25;
    get_opened_tiles_test "get_opened_tiles test for 0" (new_grid 5 5 5) 0;
    get_opened_tiles_test "get_opened_tiles test for empty grid"
      (new_grid 0 0 0) 0;
    get_opened_tiles_test "get_opened_tiles test for 1"
      (new_grid 5 5 5 |> reveal_tile (4, 5))
      1;
    get_opened_tiles_test "get_opened_tiles test for all"
      (new_grid 1 1 0 |> reveal_tile (1, 1))
      1;
    get_flagged_test "get_flagged test for 0" (new_grid 5 5 5) 0;
    get_flagged_test "get_flagged test for empty grid" (new_grid 0 0 0) 0;
    get_flagged_test "get_flagged test for 1"
      (new_grid 5 5 5 |> flag_tile (5, 5))
      1;
    get_flagged_test "get_flagged test for all flagged"
      (new_grid 1 1 1 |> flag_tile (1, 1))
      1;
  ]

(* let get_score_of_test (description : string) (name : string) (leaderboard :
   Leaderboard.leaderboard) (expected_output : string) = description >:: fun _
   -> assert_equal expected_output (Leaderboard.get_score_of name
   leaderboard) *)

let get_score_of_test_error (description : string) (name : string)
    (leaderboard : Leaderboard.leaderboard) =
  description >:: fun _ ->
  assert_raises Not_found (fun () -> Leaderboard.get_score_of name leaderboard)

let leaderboard_tests =
  [ get_score_of_test_error "empty leaderboard" "name" (Leaderboard.empty ()) ]

let suite =
  "test suite for minesweeper"
  >::: List.flatten [ grid_tests; leaderboard_tests ]

let _ = run_test_tt_main suite
