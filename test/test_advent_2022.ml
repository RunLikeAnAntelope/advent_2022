open OUnit2
open Solutions.Two
open Solutions.Three
open Solutions.Four

let print_list converter lst =
  let add a b = a ^ ";" ^ converter b in
  match lst with
  | [] -> "[]"
  | h :: t -> "[" ^ List.fold_left add (converter h) t ^ "]"

let print_lst_lst converter lst =
  let add a b = a ^ ";" ^ print_list converter b in
  match lst with
  | [] -> "[]"
  | h :: t -> "[" ^ List.fold_left add (print_list converter h) t ^ "]"

let day_two_1_tests =
  "test suite for day2, answer 1"
  >::: [
         ( "rock rock" >:: fun _ ->
           assert_equal 4 (calculate_score 'A' 'X') ~printer:string_of_int );
         ( "paper paper" >:: fun _ ->
           assert_equal 5 (calculate_score 'B' 'Y') ~printer:string_of_int );
         ( "scissors scissors" >:: fun _ ->
           assert_equal 6 (calculate_score 'C' 'Z') ~printer:string_of_int );
         ( "rock paper" >:: fun _ ->
           assert_equal 8 (calculate_score 'A' 'Y') ~printer:string_of_int );
         ( "rock scissors" >:: fun _ ->
           assert_equal 3 (calculate_score 'A' 'Z') ~printer:string_of_int );
         ( "paper rock" >:: fun _ ->
           assert_equal 1 (calculate_score 'B' 'X') ~printer:string_of_int );
         ( "paper scissors" >:: fun _ ->
           assert_equal 9 (calculate_score 'B' 'Z') ~printer:string_of_int );
         ( "scissors rock" >:: fun _ ->
           assert_equal 7 (calculate_score 'C' 'X') ~printer:string_of_int );
         ( "scissors paper" >:: fun _ ->
           assert_equal 2 (calculate_score 'C' 'Y') ~printer:string_of_int );
         ( "read_score" >:: fun _ ->
           assert_equal 8 (acc_strat [ "A X"; "A X" ]) ~printer:string_of_int );
       ]

let day_two_2_tests =
  "test suite for day2, answer 2"
  >::: [
         ( "draw_strat" >:: fun _ ->
           assert_equal 4 (apply_strat "A Y") ~printer:string_of_int );
         ( "draw_strat" >:: fun _ ->
           assert_equal 4 (determine_strat_score 'A' 'Y') ~printer:string_of_int
         );
       ]

let day_three_1_tests =
  "test suite for day3, answer 1"
  >::: [
         ( "check_char_a" >:: fun _ ->
           assert_equal 1 (priority 'a') ~printer:string_of_int );
         ( "check_char_z" >:: fun _ ->
           assert_equal 26 (priority 'z') ~printer:string_of_int );
         ( "check_char_A" >:: fun _ ->
           assert_equal 27 (priority 'A') ~printer:string_of_int );
         ( "check_char_Z" >:: fun _ ->
           assert_equal 52 (priority 'Z') ~printer:string_of_int );
         ( "check_halves" >:: fun _ ->
           assert_equal
             [ [ 'a'; 'b' ]; [ 'c'; 'd' ] ]
             (halves [ 'a'; 'b'; 'c'; 'd' ])
             ~printer:(print_lst_lst Char.escaped) );
         ( "check_contains" >:: fun _ ->
           assert_equal 'd'
             (duplicate_item [ 'v'; 'x'; 'd'; 'c' ] [ 'a'; 'd'; 'p'; 'm' ])
             ~printer:Char.escaped );
       ]

(*Day 3, answer2 tests*)
let group_by_threee =
  assert_equal
    [ [ 'i'; 'h'; 'g' ]; [ 'f'; 'e'; 'd' ]; [ 'c'; 'b'; 'a' ] ]
    (group_by_three [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i' ])
    ~printer:(print_lst_lst Char.escaped)

let intersection_of_lists =
  assert_equal 'a'
    (intersections [ [ 'a'; 'b'; 'c' ]; [ 'z'; 'a'; 'y' ]; [ 'v'; 'n'; 'a' ] ])
    ~printer:Char.escaped

let day_three_2_tests =
  "test suite for day3, answer 2"
  >::: [
         ( "check firstn" >:: fun _ ->
           assert_equal
             [ [ 'c'; 'b'; 'a' ]; [ 'd'; 'e' ] ]
             (firstn 3 [] [ 'a'; 'b'; 'c'; 'd'; 'e' ])
             ~printer:(print_lst_lst Char.escaped) );
         ("check group by three" >:: fun _ -> group_by_threee);
         ("check duplicate lists" >:: fun _ -> intersection_of_lists);
       ]

(*Day 4, answer 1 tests*)
let split_input =
  assert_equal
    [ [ 2; 4 ]; [ 6; 8 ] ]
    (prepare_data "2-4,6-8")
    ~printer:(print_lst_lst string_of_int)

let test_fully_contained_true =
  assert_equal true
    (fully_contained [ [ 2; 8 ]; [ 3; 7 ] ])
    ~printer:string_of_bool

let test_fully_contained_true_flipped =
  assert_equal true
    (fully_contained [ [ 3; 7 ]; [ 2; 8 ] ])
    ~printer:string_of_bool

let test_fully_contained_false =
  assert_equal false
    (fully_contained [ [ 4; 8 ]; [ 3; 7 ] ])
    ~printer:string_of_bool

let test_fully_contained_false_flipped =
  assert_equal false
    (fully_contained [ [ 3; 7 ]; [ 4; 8 ] ])
    ~printer:string_of_bool

let day_four_1_tests =
  "test suite for day 4, answer 1"
  >::: [
         ("check split_input" >:: fun _ -> split_input);
         ("check fully contained true" >:: fun _ -> test_fully_contained_true);
         ("check fully contained false" >:: fun _ -> test_fully_contained_false);
         ( "check fully contained true flipped" >:: fun _ ->
           test_fully_contained_true_flipped );
         ( "check fully contained false_flipped" >:: fun _ ->
           test_fully_contained_false_flipped );
       ]

let _ = run_test_tt_main day_two_1_tests
let _ = run_test_tt_main day_two_2_tests
let _ = run_test_tt_main day_three_1_tests
let _ = run_test_tt_main day_three_2_tests
let _ = run_test_tt_main day_four_1_tests
