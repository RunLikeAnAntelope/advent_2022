let rec read_channel channel (lst : string list) =
  match input_line channel with
  | exception End_of_file -> lst
  | l -> read_channel channel (l :: lst)

let read_file filename =
  let channel = open_in filename in
  read_channel channel []
