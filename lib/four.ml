type elf = { left : int; right : int }

let prepare_data str =
  str |> String.split_on_char ','
  |> List.map (fun x -> String.split_on_char '-' x)
  |> List.map (fun x -> List.map (fun x -> int_of_string x) x)

let create_elf e = { left = List.nth e 0; right = List.nth e 1 }

let fully_contained (data : int list list) =
  let elf1 = create_elf (List.hd data) in
  let elf2 = create_elf (List.nth data 1) in
  if elf1.left >= elf2.left && elf1.right <= elf2.right then true
  else if elf2.left >= elf1.left && elf2.right <= elf1.right then true
  else false

let calculate_answer1 file =
  Utils.read_file file |> List.map prepare_data |> List.map fully_contained
  |> List.filter (fun x -> x)
  |> List.length

let answer1 file =
  "1: Number of fully contained pairs: "
  ^ string_of_int (calculate_answer1 file)
