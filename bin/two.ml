let filename = "advent2.txt"

let freebie x = if x = 'X' then 1 else if x = 'Y' then 2 else 3

let calculate_score opponent me = 
        (freebie me) + 
        if opponent = 'A' && me = 'Y' then 6
        else if opponent = 'A' && me = 'Z' then 0
        else if opponent = 'B' && me = 'X' then 0
        else if opponent = 'B' && me = 'Z' then 6
        else if opponent = 'C' && me = 'X' then 6
        else if opponent = 'C' && me = 'Y' then 0
        else 3

let rec acc_strat  lst  =
    match lst  with
    |  [] -> 0 
    | h::t -> calculate_score (String.get h 0) (String.get h 2) + acc_strat t

let rec read_strat channel (lst:string list) =
    match input_line channel with 
    |exception End_of_file -> lst
    |l -> read_strat channel (l::lst)

let read_strategy filename = let channel = open_in filename in
    read_strat channel []

let calculate_answer1 = 
    read_strategy "advent2.txt"
    |> acc_strat

let answer1 = "1: Strategy Score = " ^ (string_of_int (calculate_answer1))
