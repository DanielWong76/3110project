open OUnit2
open Minesweeper
open Grid

(** Test Plan Files located in bin (main.ml) were manually tested in Utop by
    running the game. Files in src (grid.ml and leaderboard.ml) were tested
    through combination of OUnit and manually.

    Tests by OUnit were developed through both glass-box testing and black-box
    testing. Were based on looking at code and also the specification given.

    The testing approach demonstrates the correctness of the system because the
    biggest rubric for seeing if it was working properly was simply playing the
    game and making sure all functionality worked as expected. By doing this, it
    simultaneously tested all modules. After playing through and testing all
    outcomes, the OUnit tests further helped ensure individual functions of the
    modules were correct in case we missed a possible bug through the
    playthrough. *)
let string_to_string string = string

let get_dimensions_test (description : string) (grid : grid)
    (expected_output : int * int) =
  description >:: fun _ -> assert_equal expected_output (get_dimensions grid)

let get_dimensions_test_x (description : string) (grid : grid)
    (expected_output : int) =
  description >:: fun _ ->
  assert_equal expected_output (get_dimensions_x grid) ~printer:string_of_int

let get_dimensions_test_y (description : string) (grid : grid)
    (expected_output : int) =
  description >:: fun _ ->
  assert_equal expected_output (get_dimensions_y grid) ~printer:string_of_int

let get_mines_test (description : string) (grid : grid) (expected_output : int)
    =
  description >:: fun _ ->
  assert_equal expected_output (get_mines grid) ~printer:string_of_int

let get_opened_tiles_test (description : string) (grid : grid)
    (expected_output : int) =
  description >:: fun _ ->
  assert_equal expected_output (get_opened_tiles grid) ~printer:string_of_int

let get_flagged_test (description : string) (grid : grid)
    (expected_output : int) =
  description >:: fun _ ->
  assert_equal expected_output (get_flagged grid) ~printer:string_of_int

let check_win_test (description : string) (grid : grid) (expected_output : bool)
    =
  description >:: fun _ ->
  assert_equal expected_output (check_win grid) ~printer:string_of_bool

let timer_test (description : string) (time : float) (expected_output : float) =
  description >:: fun _ ->
  assert_equal expected_output time ~printer:string_of_float

let determine_num_test (description : string) (grid : grid) (coords : int * int)
    (expected_output : int) =
  description >:: fun _ ->
  assert_equal expected_output
    (determine_num grid coords)
    ~printer:string_of_int

let reveal_tile_error_game_over (description : string) (coords : int * int)
    (grid : grid) =
  description >:: fun _ ->
  assert_raises Game_Over (fun () -> reveal_tile coords grid)

let reveal_tile_error_already_revealed (description : string)
    (coords : int * int) (grid : grid) =
  description >:: fun _ ->
  assert_raises Already_Revealed (fun () -> reveal_tile coords grid)

let grid_tests =
  [
    get_dimensions_test "get_dimensions test for empty" empty (0, 0);
    get_dimensions_test "get_dimensions test for 4x5" (new_grid 4 5 0) (4, 5);
    get_dimensions_test "get_dimensions test for 8x10" (new_grid 8 10 6) (8, 10);
    get_dimensions_test "get_dimensions test for 3x3" (new_grid 3 3 3) (3, 3);
    get_dimensions_test_x "get_dimensions_x test for 4x5" (new_grid 4 5 0) 4;
    get_dimensions_test_x "get_dimensions_x test for empty" empty 0;
    get_dimensions_test_x "get_dimensions_x test for 7x5" (new_grid 7 5 5) 7;
    get_dimensions_test_x "get_dimensions_x test for 0x0" (new_grid 0 0 0) 0;
    get_dimensions_test_y "get_dimensions_y test for empty" empty 0;
    get_dimensions_test_y "get_dimensions_y test for 0x0" (new_grid 0 0 0) 0;
    get_dimensions_test_y "get_dimensions_y test for 8x8" (new_grid 8 8 0) 8;
    get_dimensions_test_y "get_dimensions_y test for 7x5" (new_grid 7 5 5) 5;
    get_mines_test "get_mines test for empty" empty 0;
    get_mines_test "get_mines test for 8x8 w/ 10 mines" (new_grid 8 8 10) 10;
    get_mines_test "get_mines test for 14x14 w/ 40 mines" (new_grid 14 14 40) 40;
    get_mines_test "get_mines test for no mines" (new_grid 10 10 0) 0;
    get_mines_test "get_mines test for all mines" (new_grid 5 5 25) 25;
    get_opened_tiles_test "get_opened_tiles test for empty" empty 0;
    get_opened_tiles_test "get_opened_tiles test for 0" (new_grid 5 5 5) 0;
    get_opened_tiles_test "get_opened_tiles test for manual empty grid"
      (new_grid 0 0 0) 0;
    get_opened_tiles_test "get_opened_tiles test for all"
      (new_grid 1 1 0 |> reveal_tile (1, 1))
      1;
    get_flagged_test "get_flagged test for empty" empty 0;
    get_flagged_test "get_flagged test for 0" (new_grid 5 5 5) 0;
    get_flagged_test "get_flagged test for empty grid" (new_grid 0 0 0) 0;
    get_flagged_test "get_flagged test for 1"
      (new_grid 5 5 5 |> flag_tile (5, 5))
      1;
    get_flagged_test "get_flagged test for all flagged"
      (new_grid 1 1 1 |> flag_tile (1, 1))
      1;
    check_win_test "check_win test for win"
      (new_grid 1 1 0 |> reveal_tile (1, 1))
      true;
    check_win_test "check_win test for loss" (new_grid 100 100 0) false;
    timer_test "empty timer test" (get_time_taken empty) 0.0;
    timer_test "add time to empty timer test"
      (get_time_taken (add_time empty 14.4))
      14.4;
    timer_test "multiple add time to timer test"
      (get_time_taken
         (let x = add_time empty 2.0 in
          add_time x 3.0))
      5.0;
    determine_num_test "determine num of empty" empty (0, 0) 0;
    determine_num_test "determine num when no mines" (new_grid 10 10 0) (0, 0) 0;
    determine_num_test "determine num when 1 mine" (new_grid 1 1 1) (0, 0) 1;
    reveal_tile_error_game_over "reveal_tile lose" (1, 1) (new_grid 10 10 100);
    reveal_tile_error_already_revealed "reveal_tile already revealed" (5, 5)
      (new_grid 10 10 0 |> reveal_tile (5, 5));
  ]

let get_score_of_test_error (description : string) (name : string)
    (leaderboard : Leaderboard.leaderboard) =
  description >:: fun _ ->
  assert_raises Not_found (fun () -> Leaderboard.get_score_of name leaderboard)

let create_score_test (description : string) (name : string) (score : int)
    (time : float) (tiles : int) (expected_output : string) =
  description >:: fun _ ->
  assert_equal expected_output
    (Leaderboard.score_to_string
       (Leaderboard.create_score name score time tiles))
    ~printer:string_to_string

let return_top_n_test (description : string) (number : int)
    (leaderboard : Leaderboard.leaderboard) (expected_output : string) =
  description >:: fun _ ->
  assert_equal expected_output
    (Leaderboard.return_top_n number leaderboard)
    ~printer:string_to_string

let get_score_of_test (description : string) (name : string)
    (leaderboard : Leaderboard.leaderboard) (expected_output : string) =
  description >:: fun _ ->
  assert_equal expected_output
    (Leaderboard.get_score_of name leaderboard)
    ~printer:string_to_string

let size_test (description : string) (leaderboard : Leaderboard.leaderboard)
    (expected_output : int) =
  description >:: fun _ ->
  assert_equal expected_output
    (Leaderboard.size leaderboard)
    ~printer:string_of_int

let export_leaderboard_test (description : string)
    (leaderboard : Leaderboard.leaderboard) (expected_output : string) =
  description >:: fun _ ->
  assert_equal expected_output
    (Leaderboard.export_leaderboard leaderboard)
    ~printer:string_to_string

let leaderboard_tests =
  [
    get_score_of_test_error "empty leaderboard" "name" (Leaderboard.empty ());
    ( "leaderboard with two entries displays both entries with return_top_n"
    >:: fun _ ->
      let score1 = Leaderboard.create_score "bob" 30 10. 20 in
      let score2 = Leaderboard.create_score "alicia" 20 15. 1 in
      let leaderboard =
        Leaderboard.empty ()
        |> Leaderboard.add_score score1
        |> Leaderboard.add_score score2
        |> Leaderboard.return_top_n 10
      in
      assert_equal
        "\n\
         1. bob, Score: 30, Time Taken: 10. s, Tiles Opened: 20\n\
         2. alicia, Score: 20, Time Taken: 15. s, Tiles Opened: 1\n"
        leaderboard ~printer:string_to_string );
    create_score_test "create score test" "Bob" 9 9.9 8
      "Bob, Score: 9, Time Taken: 9.9 s, Tiles Opened: 8";
    create_score_test "create score test 0" "" 0 0.0 0
      ", Score: 0, Time Taken: 0. s, Tiles Opened: 0";
    return_top_n_test "return_top_n_test with empty leaderboard" 1
      (Leaderboard.empty ()) "\n";
    return_top_n_test "return_top_n_test with leaderboard with just one" 1
      (Leaderboard.empty ()
      |> Leaderboard.add_score (Leaderboard.create_score "Bob" 9 9.9 9))
      "\n1. Bob, Score: 9, Time Taken: 9.9 s, Tiles Opened: 9\n";
    return_top_n_test "return_top_n_test with leaderboard with multiple entries"
      0
      (Leaderboard.empty ()
      |> Leaderboard.add_score (Leaderboard.create_score "Bob" 9 9.9 9)
      |> Leaderboard.add_score (Leaderboard.create_score "Bobby" 0 0.100 0)
      |> Leaderboard.add_score (Leaderboard.create_score "Bobby" 0 0.100 0))
      "\n1. Bob, Score: 9, Time Taken: 9.9 s, Tiles Opened: 9\n";
    get_score_of_test "get_score_of_test with leaderboard with just one" "Bob"
      (Leaderboard.empty ()
      |> Leaderboard.add_score (Leaderboard.create_score "Bob" 9 9.9 9))
      "Bob, Score: 9, Time Taken: 9.9 s, Tiles Opened: 9";
    get_score_of_test "get_score_of_test with leaderboard with multiple entries"
      "Bob"
      (Leaderboard.empty ()
      |> Leaderboard.add_score (Leaderboard.create_score "Bobby" 0 0.100 0)
      |> Leaderboard.add_score (Leaderboard.create_score "Bobbus" 6 7. 7)
      |> Leaderboard.add_score (Leaderboard.create_score "Bob" 9 9.9 9))
      "Bob, Score: 9, Time Taken: 9.9 s, Tiles Opened: 9";
    size_test "size_test with empty leaderboard" (Leaderboard.empty ()) 0;
    size_test "size_test with leaderboard with one entry"
      (Leaderboard.empty ()
      |> Leaderboard.add_score (Leaderboard.create_score "Bob" 9 9.9 9))
      1;
    size_test "size_test with leaderboard with multiple entries"
      (Leaderboard.empty ()
      |> Leaderboard.add_score (Leaderboard.create_score "Bob" 9 9.9 9)
      |> Leaderboard.add_score (Leaderboard.create_score "Bobby" 9 9.9 9)
      |> Leaderboard.add_score (Leaderboard.create_score "Bobbus" 0 0.100 0)
      |> Leaderboard.add_score (Leaderboard.create_score "Bobbeth" 0 0.100 0))
      4;
    export_leaderboard_test "export_leaderboard_test with empty leaderboard"
      (Leaderboard.empty ()) "Leaderboard\n0\nLeaderboard End\n";
    export_leaderboard_test "export_leaderboard_test with 1 entry"
      (Leaderboard.empty ()
      |> Leaderboard.add_score (Leaderboard.create_score "Bob" 9 9.9 9))
      "Leaderboard\n1\n1. Bob 9 9.9 9\nLeaderboard End\n";
    export_leaderboard_test "export_leaderboard_test with multiple entries"
      (Leaderboard.empty ()
      |> Leaderboard.add_score (Leaderboard.create_score "Bob" 9 9.9 9)
      |> Leaderboard.add_score (Leaderboard.create_score "Bobbus" 0 0.100 0))
      "Leaderboard\n2\n1. Bob 9 9.9 9\n2. Bobbus 0 0.1 0\nLeaderboard End\n";
  ]

let suite =
  "test suite for minesweeper"
  >::: List.flatten [ grid_tests; leaderboard_tests ]

let _ = run_test_tt_main suite
