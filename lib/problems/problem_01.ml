open Core

let year = 2024
let day = 1

let parse (line : string) : int * int =
  match String.split ~on:' ' line |> List.filter ~f:(Fn.non String.is_empty) with
  | [ a; b ] -> int_of_string a, int_of_string b
  | _ -> failwith ""
;;

let transpose (l : (int * int) list) : int list * int list =
  let accl (acc1, acc2) (x1, x2) = x1 :: acc1, x2 :: acc2 in
  List.fold l ~init:([], []) ~f:accl
;;

let sortint (l : int list) = List.sort l ~compare:Int.compare

let zipdiff (a : int list) (b : int list) : int list =
  List.zip_exn a b |> List.map ~f:(fun (x, y) -> Int.abs (x - y))
;;

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let la, lb = String.split_lines input |> List.map ~f:parse |> transpose in
    let sa, sb = sortint la, sortint lb in
    let res = zipdiff sa sb |> List.fold ~init:0 ~f:( + ) in
    Ok (string_of_int res)
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let la, lb = String.split_lines input |> List.map ~f:parse |> transpose in
    let ht = Hashtbl.create (module Int) in
    let () = List.iter ~f:(fun x -> Hashtbl.incr ht x) lb in
    let sim =
      List.map
        ~f:(fun x ->
          match Hashtbl.find ht x with
          | Some v -> v * x
          | None -> 0)
        la
    in
    let res = List.fold ~init:0 ~f:( + ) sim in
    Ok (string_of_int res)
  ;;
end