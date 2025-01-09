open Core

let year = 2024
let day = 7

type equation =
  { target : int
  ; operands : int list
  }

let parse input =
  let parse_line line : equation =
    let tokens = String.split line ~on:' ' in
    match tokens with
    | hd :: tl ->
      let target = String.slice hd 0 (String.length hd - 1) |> int_of_string in
      let operands = List.map tl ~f:int_of_string in
      { target; operands }
    | [] -> failwith ""
  in
  String.split_lines input |> List.map ~f:parse_line
;;

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let lst = parse input in
    let get_calib { target; operands } : int option =
      let rec aux (acc : int) (l : int list) target =
        match l with
        | [] -> target = acc
        | hd :: tl -> aux (acc + hd) tl target || aux (acc * hd) tl target
      in
      let solvable = aux (List.hd_exn operands) (List.tl_exn operands) target in
      if solvable then Some target else None
    in
    let res =
      List.map lst ~f:get_calib
      |> List.filter_map ~f:Fn.id
      |> List.sum (module Int) ~f:Fn.id
    in
    Ok (string_of_int res)
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let lst = parse input in
    let concat x y = int_of_string (string_of_int x ^ string_of_int y) in
    let get_calib { target; operands } : int option =
      let rec aux (acc : int list) (l : int list) : int list =
        match l with
        | [] -> acc
        | hd :: tl ->
          let adds = List.map acc ~f:(fun x -> x + hd) in
          let muls = List.map acc ~f:(fun x -> x * hd) in
          let cons = List.map acc ~f:(fun x -> concat x hd) in
          List.concat [ aux adds tl; aux muls tl; aux cons tl ]
      in
      let values = aux [ List.hd_exn operands ] (List.tl_exn operands) in
      let solvable = List.exists values ~f:(Int.equal target) in
      if solvable then Some target else None
    in
    let res =
      List.map lst ~f:get_calib
      |> List.filter_map ~f:Fn.id
      |> List.sum (module Int) ~f:Fn.id
    in
    Ok (string_of_int res)
  ;;
end