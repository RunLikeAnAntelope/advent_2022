exception MalformedTextFile of string
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

(*Could just return scores with these strats but whatevs*)
let win_strat opponent = 
    match opponent with
    |'A' -> 'Y'
    |'B' -> 'Z'
    |'C' -> 'X'
    | _ -> raise(MalformedTextFile "MalformedTextFile")

let lose_strat opponent =
    match opponent with 
    |'A' -> 'Z'
    |'B' -> 'X'
    |'C' -> 'Y'
    | _ -> raise(MalformedTextFile "MalformedTextFile")

let draw_strat opponent =
    match opponent with 
    |'A' -> 'X'
    |'B' -> 'Y'
    |'C' -> 'Z'
    | _ -> raise(MalformedTextFile "MalformedTextFile")

let determine_strat_score opponent strat = 
    match strat with 
        |'X' -> calculate_score opponent (lose_strat opponent)
        |'Y' -> calculate_score opponent (draw_strat opponent)
        |'Z' -> calculate_score opponent (win_strat opponent)
        |_ -> raise(MalformedTextFile "MalformedTextFile")

let apply_strat str = determine_strat_score (String.get str 0) (String.get str 2)

let rec read_strat channel (lst:string list) =
    match input_line channel with 
    |exception End_of_file -> lst
    |l -> read_strat channel (l::lst)

let read_strategy filename = let channel = open_in filename in
    read_strat channel []

let calculate_answer1 file = 
    read_strategy file 
    |> acc_strat

let calculate_answer2 file =
    read_strategy file
    |> List.map apply_strat 
    |> List.fold_left(fun acc x -> acc + x) 0

let answer1 file = "1: Strategy Score = " ^ (string_of_int (calculate_answer1 file))
let answer2 file = "2: New Strategy Score = " ^ (string_of_int(calculate_answer2(file)))
