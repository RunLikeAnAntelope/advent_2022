let file = "advent.txt"

(*Day One, question One Answer*)
let rec max_calories inc curVal maxVal =
    match input_line inc with
    | "" -> max_calories inc 0 (max curVal maxVal)
    | l -> max_calories inc (int_of_string l + curVal) maxVal
    | exception End_of_file -> max curVal maxVal 

let most_calories = let channel = open_in file in
    max_calories channel 0 0

let rec acc_elves channel acc lst =
    match input_line channel with
    | "" -> acc_elves channel 0 (acc::lst)
    | l -> acc_elves channel (acc + int_of_string l) lst
    | exception End_of_file -> lst 

let read_elves filename = let channel = open_in filename in
        acc_elves channel 0 []

let rec print_lst lst =
    match lst with 
    |[] -> []
    | h :: t -> print_endline (string_of_int(h));
        print_lst t

let rec first_n n lst =
    match lst with 
        |[]-> []
        | h :: t -> if n = 1 then [h] else h :: first_n (n-1) t

let answer1 () = print_string "    1: Most calories caried by on elf is ";
    print_int most_calories;
    print_endline ""

let answer2 () = 
    let sorted_list = List.sort Stdlib.compare (read_elves file) in 
    let rev_sorted_list = List.rev sorted_list in
    let first_3 = first_n 3 rev_sorted_list in 
    let sum lst = List.fold_left(fun acc x -> acc + x) 0 lst in
    let answer = sum first_3 in
    print_string "    2: Calories carried by top 3 elves is ";
    print_int answer


