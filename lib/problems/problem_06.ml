open Core

let year = 2024
let day = 6

type dir =
  | Left
  | Right
  | Up
  | Down

type grid =
  { data : char array
  ; nrow : int
  ; ncol : int
  }

let idx g x y = x + (y * g.ncol)
let xy g i = i % g.ncol, i / g.ncol

let next x y d =
  match d with
  | Left -> x - 1, y
  | Right -> x + 1, y
  | Up -> x, y - 1
  | Down -> x, y + 1
;;

let rotate d =
  match d with
  | Left -> Up
  | Up -> Right
  | Right -> Down
  | Down -> Left
;;

(* Alternatively @@deriving hash for (int * dir) but this is easier :^) *)
let guard_state idx d : int =
  let dn =
    match d with
    | Left -> 0
    | Up -> 1
    | Right -> 2
    | Down -> 3
  in
  (4 * idx) + dn
;;

let is_out g x y = x < 0 || x >= g.ncol || y < 0 || y >= g.nrow
let is_blocked g x y = Char.equal '#' g.data.(idx g x y)

let parse input : grid =
  let lines = String.split_lines input |> List.map ~f:String.to_list in
  let nrow = List.length lines in
  let ncol = List.length (List.nth_exn lines 0) in
  { data = Array.of_list (List.concat lines); nrow; ncol }
;;

let find_start g =
  let start, _ = Array.findi_exn g.data ~f:(fun _ x -> Char.equal x '^') in
  start
;;

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let g = parse input in
    let start = find_start g in
    let visited = Hash_set.create (module Int) in
    let x, y = xy g start in
    let rec step x y d =
      Hash_set.add visited (idx g x y);
      let nx, ny = next x y d in
      if is_out g nx ny (* Exited the map *)
      then Hash_set.length visited
      else if is_blocked g nx ny (* Next step is obstacle *)
      then step x y (rotate d)
      else (* Move forward *)
        step nx ny d
    in
    let res = step x y Up in
    Ok (string_of_int res)
  ;;
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let g = parse input in
    let start = find_start g in
    let check_loop sx sy ox oy =
      let visited = Hash_set.create (module Int) in
      let is_blocked_new x y = is_blocked g x y || (x = ox && y = oy) in
      let rec aux x y d visited =
        let seen = Hash_set.mem visited (guard_state (idx g x y) d) in
        match seen with
        | true -> true
        | false ->
          let nx, ny = next x y d in
          if is_out g nx ny (* Exited the map *)
          then false
          else if is_blocked_new nx ny (* Next step is obstacle *)
          then aux x y (rotate d) visited
          else (
            (* Move forward *)
            let () = Hash_set.add visited (guard_state (idx g x y) d) in
            aux nx ny d visited)
      in
      aux sx sy Up visited
    in
    let loops =
      Array.mapi g.data ~f:(fun i x ->
        match x with
        | '#' | '^' -> false (* Spot not empty *)
        | _ ->
          let sx, sy = xy g start in
          let ox, oy = xy g i in
          check_loop sx sy ox oy)
    in
    let res = Array.count ~f:Fn.id loops in
    Ok (string_of_int res)
  ;;
end