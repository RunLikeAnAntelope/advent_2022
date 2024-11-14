    open OUnit2
    open Solutions.Two 

    let tests = "test suite for day2, answer 2" >:::[
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

    let tests2 = "test suite for day2, answer 2" >:::[
        "draw_strat" >:: (fun _ -> assert_equal 4 (apply_strat "A Y") ~printer:string_of_int );
        "draw_strat" >:: (fun _ -> assert_equal 4 (determine_strat_score 'A' 'Y') ~printer:string_of_int );
    ]
    let _ = run_test_tt_main tests
    let _ = run_test_tt_main tests2
