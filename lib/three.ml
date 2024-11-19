exception MalformedTextFile of string
exception HalvesError of unit
exception NoDuplicate of unit

(*Answer 1 stuff*)
let priority item =
  let ascii_code = Char.code item in
  if ascii_code > 96 && ascii_code < 123 then ascii_code - 96
  else if ascii_code > 64 && ascii_code < 91 then ascii_code - 38
  else raise (MalformedTextFile "Char that is not a-z or A-Z was not found")

let halves lst =
  let rec rec_halves first_size first_half second_half =
    match first_size with
    | 0 -> [ List.rev first_half; second_half ]
    | _ -> (
        match second_half with
        | [] -> raise (HalvesError ())
        | h :: t -> rec_halves (first_size - 1) (h :: first_half) t)
  in
  rec_halves (List.length lst / 2) [] lst

let rec duplicate_item left right =
  match left with
  | [] -> raise (NoDuplicate ())
  | h :: t -> if List.mem h right then h else duplicate_item t right

let ruck_answer lst =
  match lst with
  | [] -> raise (MalformedTextFile "empty line in input")
  | h :: t -> duplicate_item h (List.hd t)

(*Answer 2 stuff*)
let rec firstn n ret lst =
  match n with
  | 0 -> [ ret; lst ]
  | _ -> (
      match lst with
      | [] -> raise (MalformedTextFile "Should be groups of three")
      | h :: t -> firstn (n - 1) (h :: ret) t)

let group_by_three lst =
  let rec group_by_three acc lst =
    if lst = [] then acc
    else
      let firstn_and_tail = firstn 3 [] lst in
      match firstn_and_tail with
      | [] -> raise (MalformedTextFile "Something bad. Not sure what though")
      | h :: t -> group_by_three (h :: acc) (List.hd t)
  in
  group_by_three [] lst

let intersection lst1 lst2 =
  let rec intersection lst1 lst2 answer =
    match lst1 with
    | [] -> answer
    | h :: t ->
        if List.mem h lst2 then intersection t lst2 (h :: answer)
        else intersection t lst2 answer
  in
  intersection lst1 lst2 []

(*Trusting I am getting good input*)
let intersections lst =
  let rec intersections lst acc =
    match lst with
    | [] -> List.hd acc
    | h :: [] -> List.hd (intersection h acc)
    | h :: t -> intersections (List.tl t) (intersection h (List.hd t))
  in
  intersections lst []

let rec read_channel channel (lst : string list) =
  match input_line channel with
  | exception End_of_file -> lst
  | l -> read_channel channel (l :: lst)

let read_file filename =
  let channel = open_in filename in
  read_channel channel []

let calculate_answer1 file =
  read_file file
  |> List.map (fun x ->
         x |> String.to_seq |> List.of_seq |> halves |> ruck_answer |> priority)
  |> List.fold_left (fun acc x -> acc + x) 0

let calculate_answer2 file =
  read_file file
  |> List.map (fun x -> x |> String.to_seq |> List.of_seq)
  |> group_by_three
  |> List.map (fun x -> x |> intersections |> priority)
  |> List.fold_left (fun acc x -> acc + x) 0

let answer1 file =
  "1: Sum of priorites = " ^ string_of_int (calculate_answer1 file)

let answer2 file =
  "1: Sum of priorites for badges = " ^ string_of_int (calculate_answer2 file)
