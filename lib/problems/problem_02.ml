open Core

let year = 2024
let day = 2

let parse (input : string) : int list list =
  let parse_line (line : string) : int list =
    String.split ~on:' ' line |> List.map ~f:int_of_string
  in
  String.split_lines input |> List.map ~f:parse_line
;;

let rec delta (lst : int list) : int list =
  match lst with
  | x :: y :: xs -> (y - x) :: delta (y :: xs)
  | _ :: [] -> []
  | [] -> []
;;

let check_safe (lst : int list) : bool =
  let deltas = delta lst in
  let signs_ok =
    List.for_all deltas ~f:(fun x -> x > 0) || List.for_all deltas ~f:(fun x -> x < 0)
  in
  let bounds_ok =
    List.for_all deltas ~f:(fun x ->
      let z = abs x in
      z >= 1 && z <= 3)
  in
  signs_ok && bounds_ok
;;

(* replicate [x; y; z] = [[x; y; z]; [x; y; z]; [x; y; z]] *)
let replicate (lst : int list) : int list list =
  let rec repeat (acc : int list list) (l : int list) (n : int) =
    match n with
    | 0 -> acc
    | _ -> l :: repeat acc l (n - 1)
  in
  repeat [] lst (List.length lst)
;;

let remove_nth (idx : int) (lst : int list) : int list =
  let rec rem (idx : int) (lst : int list) =
    match lst with
    | [] -> []
    | _ :: tl when idx = 0 -> tl
    | hd :: tl -> hd :: rem (idx - 1) tl
  in
  rem idx lst
;;

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let res = parse input |> List.map ~f:check_safe |> List.count ~f:Fun.id in
    Ok (string_of_int res)
  ;;
end

module Part_2 = struct
  let check_damp_safe (lst : int list) =
    replicate lst
    |> List.mapi ~f:remove_nth
    |> List.map ~f:check_safe
    |> List.exists ~f:Fun.id
  ;;

  let run (input : string) : (string, string) result =
    let damp_cnt = parse input |> List.map ~f:check_damp_safe in
    let safe_cnt = parse input |> List.map ~f:check_safe in
    let res =
      List.zip_exn damp_cnt safe_cnt
      |> List.map ~f:(fun (x, y) -> x || y)
      |> List.count ~f:Fun.id
    in
    Ok (string_of_int res)
  ;;
end