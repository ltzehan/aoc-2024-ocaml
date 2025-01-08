open Core

let year = 2024
let day = 3

type command =
  | Product of int
  | Start
  | Stop

type acct =
  | Running of int
  | Stopped of int

let parse_mul (str : string) : int =
  let split_idx = String.index_exn str ',' in
  let v1 = String.slice str 4 split_idx in
  let v2 = String.slice str (split_idx + 1) (String.length str - 1) in
  int_of_string v1 * int_of_string v2
;;

let find_all_captures regex (text : string) : command list =
  let matches = Re2.find_all_exn regex text in
  let find_captures (str : string) : command =
    if String.is_prefix str ~prefix:"mul"
    then Product (parse_mul str)
    else if String.is_prefix str ~prefix:"don't"
    then Stop
    else Start
  in
  List.map ~f:find_captures matches
;;

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let regex = Re2.create_exn "mul\\(\\d+,\\d+\\)" in
    let unwrap (x : command) : int =
      match x with
      | Product v -> v
      | _ -> failwith ""
    in
    let res = find_all_captures regex input |> List.sum (module Int) ~f:unwrap in
    Ok (string_of_int res)
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let regex = Re2.create_exn "(mul\\(\\d+,\\d+\\)|don't\\(\\)|do\\(\\))" in
    let unwrap (x : acct) : int =
      match x with
      | Running v -> v
      | Stopped v -> v
    in
    let fold_cmd (acc : acct) (cmd : command) : acct =
      match cmd with
      | Start -> Running (unwrap acc)
      | Stop -> Stopped (unwrap acc)
      | Product v ->
        (match acc with
         | Running s -> Running (s + v)
         | Stopped s -> Stopped s)
    in
    let res = find_all_captures regex input |> List.fold ~init:(Running 0) ~f:fold_cmd in
    Ok (unwrap res |> string_of_int)
  ;;
end