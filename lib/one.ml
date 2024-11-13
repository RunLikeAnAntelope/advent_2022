(*Day One, question One Answer*)
let rec max_calories inc curVal maxVal =
    match input_line inc with
    | "" -> max_calories inc 0 (max curVal maxVal)
    | l -> max_calories inc (int_of_string l + curVal) maxVal
    | exception End_of_file -> max curVal maxVal 

let most_calories file = let channel = open_in file in
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

let answer1 file =  "1: Most calories caried by on elf is " ^ (string_of_int (most_calories file))

let answer2  file = 
    let int_answer_2 =
        read_elves file
        |> List.sort Stdlib.compare
        |> List.rev
        |> first_n 3
        |> List.fold_left(fun acc x -> acc + x) 0  in
    "2: Calories carried by top 3 elves is " ^ (string_of_int int_answer_2) 


