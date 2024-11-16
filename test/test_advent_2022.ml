open OUnit2
open Solutions.Two 
open Solutions.Three
let rec print_list lst = 
    match lst with
    | [] -> "" 
    | h::t -> Char.escaped h ^ ";"^ print_list t

let rec print_lst_lst lst = 
    match lst with
    | [] -> "" 
    | h::t -> print_list h ^ "|" ^ print_lst_lst t

let day_two_1_tests = "test suite for day2, answer 1" >:::[
    "rock rock" >:: (fun _ -> assert_equal 4 (calculate_score 'A' 'X') ~printer:string_of_int );
    "paper paper" >:: (fun _ -> assert_equal 5 (calculate_score 'B' 'Y') ~printer:string_of_int );
    "scissors scissors" >:: (fun _ -> assert_equal 6 (calculate_score 'C' 'Z') ~printer:string_of_int );
    "rock paper" >:: (fun _ -> assert_equal 8 (calculate_score 'A' 'Y') ~printer:string_of_int );
    "rock scissors" >:: (fun _ -> assert_equal 3 (calculate_score 'A' 'Z') ~printer:string_of_int );
    "paper rock" >:: (fun _ -> assert_equal 1 (calculate_score 'B' 'X') ~printer:string_of_int );
    "paper scissors" >:: (fun _ -> assert_equal 9 (calculate_score 'B' 'Z') ~printer:string_of_int );
    "scissors rock" >:: (fun _ -> assert_equal 7 (calculate_score 'C' 'X') ~printer:string_of_int );
    "scissors paper" >:: (fun _ -> assert_equal 2 (calculate_score 'C' 'Y') ~printer:string_of_int );
    "read_score" >:: (fun _ -> assert_equal 8 (acc_strat ["A X";"A X"]) ~printer:string_of_int );
]

let day_two_2_tests = "test suite for day2, answer 2" >:::[
    "draw_strat" >:: (fun _ -> assert_equal 4 (apply_strat "A Y") ~printer:string_of_int );
    "draw_strat" >:: (fun _ -> assert_equal 4 (determine_strat_score 'A' 'Y') ~printer:string_of_int );
]

(*Make printer for halves*)
let day_three_1_tests = "test suite for day3, answer 1" >:::[
    "check_char_a" >:: (fun _ -> assert_equal 1 (priority 'a') ~printer:string_of_int );
    "check_char_z" >:: (fun _ -> assert_equal 26 (priority 'z') ~printer:string_of_int );
    "check_char_A" >:: (fun _ -> assert_equal 27 (priority 'A') ~printer:string_of_int );
    "check_char_Z" >:: (fun _ -> assert_equal 52 (priority 'Z') ~printer:string_of_int );
    "check_halves" >:: (fun _ -> assert_equal ([['a';'b'] ; ['c';'d']]) (halves ['a';'b';'c';'d'])~printer:print_lst_lst);
    "check_contains" >:: (fun _ -> assert_equal 'd' (duplicate_item ['v';'x';'d';'c'] ['a';'d';'p';'m']) ~printer:Char.escaped );
]
let _ = run_test_tt_main day_two_1_tests
let _ = run_test_tt_main day_two_2_tests 
let _ = run_test_tt_main day_three_1_tests
