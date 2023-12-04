let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let is_digit alpha = match alpha with '0' .. '9' -> true | _ -> false
let append_char string char = string ^ String.make 1 char
let prepend_char string char = String.make 1 char ^ string

let find_first_last token =
  let result = ref "" in
  let i = ref 0 in
  let j = ref (String.length token - 1) in
  let i_found = ref false in
  let j_found = ref false in
  while (not !i_found) || not !j_found do
    (if not !i_found then
     let start_char = String.get token !i in
     if is_digit start_char then (
       result := prepend_char !result start_char;
       i_found := true));

    (if not !j_found then
     let finish_char = String.get token !j in
     if is_digit finish_char then (
       result := append_char !result finish_char;
       j_found := true));

    i := !i + 1;
    j := !j - 1
  done;
  int_of_string !result

let () =
  let sum = ref 0 in
  let lines = read_file "./samples/day1.sample" in
  let rec process_line list =
    match list with
    | [] -> ()
    | head :: body ->
        let result = find_first_last head in
        sum := !sum + result;
        process_line body
  in
  process_line lines;
  let sum_str = string_of_int !sum in
  print_endline sum_str
